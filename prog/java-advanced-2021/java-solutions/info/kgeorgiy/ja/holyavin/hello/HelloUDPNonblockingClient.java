package info.kgeorgiy.ja.holyavin.hello;

import java.io.Closeable;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.*;
import java.nio.ByteBuffer;
import java.nio.channels.*;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

public class HelloUDPNonblockingClient extends ClientProgram implements info.kgeorgiy.java.advanced.hello.HelloClient {
    // no additional allocations inside after creation
    private static class ChannelAttachment extends ClientHandler {
        final ByteBuffer response;
        final ByteBuffer request;

        ChannelAttachment(final int threadId, final DatagramSocket socket) throws SocketException {
            super(threadId, socket);
            this.response = Util.allocBuffer(socket);
            this.request = Util.allocBuffer(socket);
        }

        @Override
        protected void beginImpl(final DatagramSocket socket) {
            request.clear();
            encoder.encode(requestString, request, true);
            Util.rewindBuffer(request);
        }

        @Override
        protected void sendImpl(final DatagramSocket socket, final SocketAddress address) {
            final DatagramChannel channel = socket.getChannel();
            try {
                request.position(0);
                channel.send(request, address);
            } catch (final IOException e) {
                throw new UncheckedIOException(e);
            }
        }

        @Override
        protected boolean receiveImpl(final DatagramSocket socket) throws IOException {
            final DatagramChannel channel = socket.getChannel();
            response.clear();
            channel.read(response);  // no address returned
            decoder.decode(Util.rewindBuffer(response), responseString, true);
            return true;
        }
    }

    @Override
    public void run(final String host, final int port, final String prefix, final int threads, final int requests) {
        // allocate memory
        final InetSocketAddress address = resolveAddress(host, port);

        final AtomicInteger counter = new AtomicInteger();
        final Consumer<SelectionKey> action = key -> {
            // no allocations here
            final DatagramChannel channel = (DatagramChannel) key.channel();
            final ChannelAttachment attachment = (ChannelAttachment) key.attachment();

            if ((key.interestOps() & SelectionKey.OP_WRITE) != 0) {
                attachment.begin(channel.socket(), prefix);
                if (key.isWritable()){
                    attachment.send(channel.socket(), address);
                    key.interestOps(SelectionKey.OP_READ);
                }
            } else {
                if (key.isReadable()) {
                    if (attachment.receive(channel.socket()) && attachment.requestId == requests) {
                        key.cancel();
                        counter.incrementAndGet();
                    } else {
                        key.interestOps(SelectionKey.OP_WRITE);
                    }
                }
            }
        };

        final HelloUDPError exc = new HelloUDPError("Error occurred while sending requests");
        final List<DatagramChannel> channels = new ArrayList<>();
        try (final Closeable tidyUp = () -> Util.closeList(channels);
             final Selector selector = Selector.open()) {
            for (int i = 0; i < threads; i++) {
                final DatagramChannel channel = DatagramChannel.open();
                channels.add(channel);
                channel.connect(address);
                channel.configureBlocking(false);
                channel.register(selector, SelectionKey.OP_WRITE, new ChannelAttachment(i, channel.socket()));
            }

            // -- allocation ended --
            while (!Thread.interrupted() && counter.get() != threads) {
                try {
                    if (selector.select(action, SO_TIMEOUT) == 0) {
                        for (final var key : selector.keys()) {
                            if (key.isValid() && (key.interestOps() & SelectionKey.OP_READ) != 0) {
                                key.interestOps(SelectionKey.OP_WRITE);
                            }
                        }
                    }
                } catch (final UncheckedIOException e) {
                    throw e.getCause();
                }
            }
        } catch (final IOException e) {
            exc.initCause(e);
            throw exc;
        }
    }

    public static void main(final String[] args) {
        ClientProgram.main(HelloUDPNonblockingClient::new, args);
    }
}
