package info.kgeorgiy.ja.holyavin;

import java.io.*;
import java.net.Socket;
import java.net.SocketException;
import java.util.Arrays;
import java.util.Objects;
import java.util.function.Consumer;

public class ChatClientProgram implements ChatClient {
    private ObjectOutputStream output;
    private Socket socket;

    private void writeObject(final Object object) throws IOException {
        output.writeObject(object);
        output.flush();
    }

    @Override
    public void connect(final String host, final int port, final String userName, final Consumer<ChatReceivedMessage> action) {
        try {
            socket = new Socket(host, port);
        } catch (final IOException e) {
            throw new ChatError("Cannot connect to host " + host + " and port " + port, e);
        }

        try {
            output = new ObjectOutputStream(socket.getOutputStream());
            output.flush();
        } catch (final IOException e) {
            throw new ChatError("I/O error occurred while establishing out connection to server", e);
        }

        final ObjectInputStream input;
        try {
            input = new ObjectInputStream(socket.getInputStream());  // do not close on exit
        } catch (final IOException e) {
            // close output
            try {
                output.close();
            } catch (final IOException exc) {
                e.addSuppressed(exc);
            }
            throw new ChatError("I/O error occurred while establishing in connection to server", e);
        }

        try {
            // send username
            writeObject(new ChatUsername(userName));
        } catch (final IOException e) {
            // close output & input
            // :NOTE: * Очень странный способ закрытия
            try (final var fOutput = output; final var fInput = input) {
            } catch (final IOException exc) {
                e.addSuppressed(exc);
            }
            throw new ChatError("I/O error occurred while establishing in connection to server", e);
        }

        // listener
        new Thread(() -> {
            while (!Thread.interrupted()) {
                final Object message;
                try {
                    // receive ChatReceivedMessage
                    message = input.readObject();
                    if (message instanceof ChatReceivedMessage) {
                        action.accept((ChatReceivedMessage) message);
                    }
                } catch (final EOFException | SocketException e) {
                    // shutdown
                    break;
                } catch (final ClassNotFoundException | IOException ignored) {
                }
            }
        }).start();
    }

    public void send(final String message) {
        if (output == null) {
            throw new ChatError("Client is closed");
        }
        // Send ChatSentMessage
        try {
            writeObject(new ChatSentMessage(message));
        } catch (final IOException e) {
            throw new ChatError("I/O error occurred while sending message", e);
        }
    }

    @Override
    public void close() throws IOException {
        // :NOTE: И здесь
        try (final var fOutput = output;
             final var fSocket = socket) {
            output = null;
            socket = null;
        }
    }

    public static void main(final String[] args) {
        final String usage = "Usage: java <client> <host> <port> <chat username>";
        if (args == null || args.length < 3 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.out.println(usage);
            return;
        }

        final String host = args[0];
        final int port;
        try {
            port = Integer.parseInt(args[1]);
        } catch (final NumberFormatException e) {
            System.err.println("Cannot parse port number: " + e);
            return;
        }
        final String username = args[2];
        try (final var client = new ChatClientProgram()) {
            client.connect(host, port, username, message -> System.out.println(message.userName + ">" + message.message));
            final BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
            String message;
            while ((message = reader.readLine()) != null) {
                client.send(message);
            }
        } catch (final ChatError e) {
            System.err.println("Chat error: " + e);
        } catch (final IOException e) {
            System.out.println("Error closing resources: " + e);
        }
    }
}
