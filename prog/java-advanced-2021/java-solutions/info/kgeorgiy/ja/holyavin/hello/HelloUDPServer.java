package info.kgeorgiy.ja.holyavin.hello;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketException;

public class HelloUDPServer extends ServerProgram implements info.kgeorgiy.java.advanced.hello.HelloServer {
    private DatagramSocket socket;

    private class ServerTask implements Runnable {
        DatagramPacket requestPacket;

        ServerTask(DatagramPacket requestPacket) {
            this.requestPacket = requestPacket;
        }

        @Override
        public void run() {
            try {
                socket.send(
                        Util.stringToPacket(transformRequest(Util.packetToString(requestPacket)),
                                requestPacket.getSocketAddress())
                );
            } catch (IOException ignored) {
            }
        }
    }

    @Override
    public void start(int port, int threads) {
        final DatagramSocket socket;
        try {
            socket = new DatagramSocket(new InetSocketAddress(port));
        } catch (SocketException e) {
            throw new HelloUDPError("Could not start server", e);
        }
        this.socket = socket;

        super.start(threads, () -> {
            while (true) {
                try {
                    DatagramPacket requestPacket = Util.allocPacket(socket);
                    socket.receive(requestPacket);
                    executorService.submit(new ServerTask(requestPacket));
                } catch (SocketException e) {
                    return;
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        });
    }

    @Override
    public void close() {
        socket.close();
        super.close();
    }

    public static void main(String[] args) {
        ServerProgram.main(HelloUDPServer::new, args);
    }
}
