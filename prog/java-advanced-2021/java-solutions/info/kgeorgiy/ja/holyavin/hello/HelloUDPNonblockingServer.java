package info.kgeorgiy.ja.holyavin.hello;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.*;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.ClosedSelectorException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class HelloUDPNonblockingServer extends ServerProgram implements info.kgeorgiy.java.advanced.hello.HelloServer {
    private DatagramChannel channel;
    private Selector selector;
    private SelectionKey channelKey;
    private BlockingQueue<DifferentPacket> requests, responses, packetsPool;

    private static class DifferentPacket {
        ByteBuffer buffer;
        SocketAddress address;

        DifferentPacket(DatagramSocket socket) throws SocketException {
            buffer = Util.allocBuffer(socket);
        }
    }

    private class ServerTask implements Callable<Void> {
        private final CharBuffer requestString;
        private final CharsetDecoder decoder = StandardCharsets.UTF_8.newDecoder();
        private final CharsetEncoder encoder = StandardCharsets.UTF_8.newEncoder();

        ServerTask(DatagramSocket socket) throws SocketException {
            requestString = Util.allocCharBuffer(socket);
        }

        @Override
        public Void call() {
            try {
                while (!Thread.interrupted()) {
                    DifferentPacket request = requests.take();  // block

                    requestString.clear().append(RESPONSE_PREFIX);
                    decoder.decode(request.buffer, requestString, true);
                    encoder.encode(Util.rewindBuffer(requestString), request.buffer.clear(), true);
                    Util.rewindBuffer(request.buffer);

                    responses.put(request);  // all packets from pool => queue cannot be full
                    if (channelKey.interestOpsOr(SelectionKey.OP_WRITE) == SelectionKey.OP_READ) {
                        selector.wakeup();
                    }
                }
            } catch (InterruptedException ignored) {
            } finally {
                Thread.currentThread().interrupt();
            }
            return null;
        }
    }

    private interface ResourceFactory<T> {
        T create() throws IOException;
    }

    private static <T> List<T> constructN(ResourceFactory<T> factory, int n) throws IOException {
        try {
            return Stream.generate(() -> {
                try {
                    return factory.create();
                } catch (IOException e) {
                    throw new UncheckedIOException(e);
                }
            }).limit(n).collect(Collectors.toUnmodifiableList());
        } catch (UncheckedIOException e) {
            throw e.getCause();
        }
    }

    @Override
    public void start(int port, int threads) {
        // allocate memory
        ByteBuffer nul;
        try {
            selector = Selector.open();
            channel = DatagramChannel.open();
            channel.bind(new InetSocketAddress(port));
            channel.configureBlocking(false);
            channelKey = channel.register(selector, SelectionKey.OP_READ);
        } catch (IOException e) {
            throw new HelloUDPError("Could not start server", e);
        }
        int queueSize = Math.max(QUEUE_SIZE, threads * 50);
        requests = new ArrayBlockingQueue<>(queueSize);
        responses = new ArrayBlockingQueue<>(queueSize);
        List<ServerTask> tasks;
        try {
            nul = Util.allocBuffer(channel.socket());
            packetsPool = new ArrayBlockingQueue<>(queueSize, true, constructN(
                    () -> new DifferentPacket(channel.socket()), queueSize
            ));
            tasks = constructN(() -> new ServerTask(channel.socket()), threads);
        } catch (IOException e) {
            throw new HelloUDPError("Could not allocate resources", e);
        }

        super.start(threads, () -> {
            for (var task : tasks) {
                executorService.submit(task);
            }
            HelloUDPError wrapper = new HelloUDPError("Wraps IOException");

            // -- allocation ended --
            try {
                while (!Thread.interrupted()) {
                    try {
                        selector.select(key -> {
                            try {
                                if ((key.interestOps() & SelectionKey.OP_WRITE) != 0 && key.isWritable()) {
                                    // response exists because of set write flag
                                    DifferentPacket packet = responses.take();
                                    channel.send(packet.buffer, packet.address);
                                    packetsPool.add(packet);  // packet from pool => pool cannot be full
                                    if (responses.isEmpty()) {
                                        key.interestOpsAnd(SelectionKey.OP_READ);
                                    }
                                } else if (key.isReadable()) {
                                    DifferentPacket packet = packetsPool.poll();
                                    if (packet != null) {
                                        packet.address = channel.receive(packet.buffer.clear());
                                        Util.rewindBuffer(packet.buffer);
                                        requests.add(packet);  // all packets are from pool => queue cannot be full
                                    } else {
                                        channel.receive(nul.clear());
                                    }
                                }
                            } catch (IOException e) {
                                wrapper.initCause(e);
                                throw wrapper;
                            } catch (InterruptedException e) {
                                Thread.currentThread().interrupt();
                            }
                        });
                    } catch (HelloUDPError e) {
                        throw (IOException) e.getCause();
                    }
                }
                Thread.currentThread().interrupt();
            } catch (ClosedSelectorException ignored) {
                channelKey.cancel();
            } catch (IOException e) {
                e.printStackTrace();
            }
        });
    }

    @Override
    public void close() {
        try {
            selector.close();
        } catch (IOException ignored) {
            // oops
        }
        try {
            channel.close();
        } catch (IOException ignored) {
            // oops
        }
        executorService.shutdownNow();
        super.close();
    }

    public static void main(String[] args) {
        ServerProgram.main(HelloUDPNonblockingServer::new, args);
    }
}
