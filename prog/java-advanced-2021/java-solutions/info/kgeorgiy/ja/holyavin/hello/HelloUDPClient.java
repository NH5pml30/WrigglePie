package info.kgeorgiy.ja.holyavin.hello;

import java.io.IOException;
import java.net.*;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;

public class HelloUDPClient extends ClientProgram implements info.kgeorgiy.java.advanced.hello.HelloClient {
    private static class ClientTask extends ClientHandler {
        DatagramPacket requestPacket, responsePacket;

        ClientTask(int threadId, DatagramSocket socket) throws SocketException {
            super(threadId, socket);
        }

        @Override
        protected void beginImpl(DatagramSocket socket) throws IOException {
            requestPacket = Util.stringToPacket(requestString.toString());
            requestString.position(requestString.limit());
            responsePacket = Util.allocPacket(socket);
        }

        @Override
        protected void sendImpl(DatagramSocket socket, SocketAddress address) throws IOException {
            socket.send(requestPacket);
        }

        @Override
        protected boolean receiveImpl(DatagramSocket socket) throws IOException {
            try {
                socket.receive(responsePacket);
            } catch (SocketTimeoutException e) {
                printRequestStatus("response", "ignored :(");
                return false;
            }
            responseString.clear().append(Util.packetToString(responsePacket));
            return true;
        }
    }

    private static Callable<Void> createClientTask(SocketAddress address, String prefix, int requests, int threadId) {
        return () -> {
            try (DatagramSocket socket = new DatagramSocket()) {
                socket.setSoTimeout(SO_TIMEOUT);
                socket.connect(address);

                ClientTask task = new ClientTask(threadId, socket);

                for (int requestId = 0; requestId < requests; requestId++) {
                    task.begin(socket, prefix);
                    while (true) {
                        task.send(socket, address);
                        if (task.receive(socket)) {
                            break;
                        }
                    }
                }
            }
            return null;
        };
    }

    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        InetSocketAddress address = resolveAddress(host, port);

        ExecutorService executorService = Executors.newFixedThreadPool(threads);
        try (Util.NothrowAutoCloseable tidyUp =
                     () -> Util.closeExecutorService(executorService)) {
            List<Callable<Void>> tasks = new ArrayList<>();
            for (int threadId = 0; threadId < threads; threadId++) {
                tasks.add(createClientTask(address, prefix, requests, threadId));
            }
            List<Future<Void>> futures = executorService.invokeAll(tasks);

            HelloUDPError exc = null;
            for (var future : futures) {
                try {
                    future.get();  // cannot throw InterruptedException because isDone.
                } catch (ExecutionException e) {
                    if (exc == null) {
                        exc = new HelloUDPError("Error occurred while sending requests", e.getCause());
                    } else {
                        exc.addSuppressed(e.getCause());
                    }
                }
            }
            if (exc != null) {
                throw exc;
            }
        } catch (InterruptedException exc) {
            Thread.currentThread().interrupt();
            throw new HelloUDPError("Interrupted while waiting for results", exc);
        }
    }

    public static void main(String[] args) {
        ClientProgram.main(HelloUDPClient::new, args);
    }
}
