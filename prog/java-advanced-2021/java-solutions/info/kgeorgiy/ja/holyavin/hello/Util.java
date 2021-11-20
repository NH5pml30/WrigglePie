package info.kgeorgiy.ja.holyavin.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;
import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.*;
import java.net.*;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;
import java.util.function.Supplier;

class HelloUDPError extends RuntimeException {
    HelloUDPError(final String message) {
        super(message);
    }

    HelloUDPError(final String message, final Throwable cause) {
        super(message, cause);
    }
}

class Util {
    static DatagramPacket stringToPacket(final String request) {
        final byte[] bytes = request.getBytes(StandardCharsets.UTF_8);
        return new DatagramPacket(bytes, bytes.length);
    }

    static DatagramPacket stringToPacket(final String request, final SocketAddress address) {
        final byte[] bytes = request.getBytes(StandardCharsets.UTF_8);
        return new DatagramPacket(bytes, bytes.length, address);
    }

    static DatagramPacket allocPacket(final DatagramSocket socket) throws SocketException {
        return new DatagramPacket(new byte[socket.getReceiveBufferSize()], socket.getReceiveBufferSize());
    }

    static ByteBuffer allocBuffer(final DatagramSocket socket) throws SocketException {
        return ByteBuffer.allocate(socket.getReceiveBufferSize());
    }

    static CharBuffer allocCharBuffer(final DatagramSocket socket) throws SocketException {
        return CharBuffer.allocate(socket.getReceiveBufferSize());
    }

    static <T extends Buffer> T rewindBuffer(final T buffer) {
        // :NOTE: Руками??
        buffer.limit(buffer.position());
        buffer.position(0);
        return buffer;
    }

    static String packetToString(final DatagramPacket packet) {
        return new String(packet.getData(), packet.getOffset(), packet.getLength(), StandardCharsets.UTF_8);
    }

    interface NothrowAutoCloseable extends AutoCloseable {
        @Override
        void close();
    }

    static void closeExecutorService(final ExecutorService service) {
        service.shutdown();
        boolean interrupted = Thread.interrupted();
        while (true) {
            try {
                if (service.awaitTermination(Long.MAX_VALUE, TimeUnit.SECONDS)) {
                    break;
                }
            } catch (final InterruptedException e) {
                service.shutdownNow();
                interrupted = true;
            }
        }
        if (interrupted) {
            Thread.currentThread().interrupt();
        }
    }

    static void closeList(final List<? extends Closeable> list) throws IOException {
        IOException exc = null;
        for (final var el : list) {
            try {
                el.close();
            } catch (final IOException e) {
                if (exc == null) {
                    exc = e;
                } else {
                    exc.addSuppressed(e);
                }
            }
        }
        if (exc != null) {
            throw exc;
        }
    }

    private static final ThreadLocal<StringBuilder> printIntBuffer = ThreadLocal.withInitial(() ->
            new StringBuilder(20));

    private static StringBuilder printIntToBuffer(final int number) {
        printIntBuffer.get().setLength(0);
        return printIntBuffer.get().append(number);
    }

    static CharBuffer printInt(final CharBuffer buffer, final int number) {
        printIntToBuffer(number).getChars(0, printIntBuffer.get().length(),
                buffer.array(), buffer.arrayOffset() + buffer.position());
        buffer.position(buffer.position() + printIntBuffer.get().length());
        return buffer;
    }

    static boolean checkIntStringEq(final CharBuffer buffer, final int start, final int end, final int number) {
        if (printIntToBuffer(number).length() != end - start) {
            return false;
        }
        for (int i = 0; i < end - start; i++) {
            if (printIntBuffer.get().charAt(i) != buffer.get(start + i)) {
                return false;
            }
        }
        return true;
    }
}

// no additional allocations inside after creation
abstract class ClientHandler {
    protected final int threadId;
    protected boolean next = true;

    protected int requestId = 0;
    protected final CharsetDecoder decoder = StandardCharsets.UTF_8.newDecoder();
    protected final CharsetEncoder encoder = StandardCharsets.UTF_8.newEncoder();
    protected final CharBuffer idString, requestString, responseString;

    private static final String LITERAL_CONTENTS = "contents";
    private static final String LITERAL_RESPONSE = "response";
    private static final String LITERAL_SKIPPED = "(skipped)";
    private static final String LITERAL_REQUEST = "Request(";

    protected ClientHandler(final int threadId, final DatagramSocket socket) throws SocketException {
        this.threadId = threadId;
        this.idString = Util.allocCharBuffer(socket);
        this.requestString = Util.allocCharBuffer(socket);
        this.responseString = Util.allocCharBuffer(socket);
    }

    abstract protected void beginImpl(DatagramSocket socket) throws IOException;
    abstract protected void sendImpl(DatagramSocket socket, SocketAddress address) throws IOException;
    abstract protected boolean receiveImpl(DatagramSocket socket) throws IOException;

    private static boolean isSimpleDigit(final char ch) {
        return ch >= '0' && ch <= '9';
    }
    private static boolean isNotSimpleDigit(final char ch) {
        return !isSimpleDigit(ch);
    }

    private void skipWhile(final Predicate<Character> test) {
        while (responseString.position() < responseString.limit()) {
            if (!test.test(responseString.get())) {
                responseString.position(responseString.position() - 1);
                return;
            }
        }
    }

    private boolean validateResponseNumber(final int number) {
        final int start = responseString.position();
        skipWhile(ClientHandler::isSimpleDigit);
        final int end = responseString.position();
        final boolean res = Util.checkIntStringEq(responseString, start, end, number);
        responseString.position(end);
        return res;
    }

    boolean validateResponseString() {
        responseString.position(0);
        skipWhile(ClientHandler::isNotSimpleDigit);
        if (!validateResponseNumber(threadId)) {
            responseString.position(0);
            return false;
        }
        skipWhile(ClientHandler::isNotSimpleDigit);
        if (!validateResponseNumber(requestId)) {
            responseString.position(0);
            return false;
        }
        skipWhile(ClientHandler::isNotSimpleDigit);
        final boolean result = responseString.position() == responseString.limit();
        responseString.position(0);
        return result;
    }

    void begin(final DatagramSocket socket, final String prefix) {
        if (next) {
            buildRequestIdString();
            buildRequestString(prefix);
            try {
                beginImpl(socket);
                requestString.position(0);
            } catch (final IOException e) {
                throw new UncheckedIOException(e);
            }
            next = false;
        }
    }

    void send(final DatagramSocket socket, final SocketAddress address) {
        printRequestStatus(LITERAL_CONTENTS, requestString);
        try {
            sendImpl(socket, address);
        } catch (final IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    boolean receive(final DatagramSocket socket) {
        try {
            responseString.clear();
            if (!receiveImpl(socket)) {
                return false;
            }
            Util.rewindBuffer(responseString);
            if (!validateResponseString()) {
                printRequestStatus(LITERAL_RESPONSE, responseString, LITERAL_SKIPPED);
                return false;
            }
            printRequestStatus(LITERAL_RESPONSE, responseString);
            requestId++;
            next = true;
            return true;
        } catch (final IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    protected PrintStream printRequestStatusHelper(final CharSequence what, final CharSequence status) {
        return System.out.append(idString).append(' ').append(what).append(':').append(' ').append(status);
    }

    protected void printRequestStatus(final CharSequence what, final CharSequence status) {
        synchronized (System.out) {
            printRequestStatusHelper(what, status).println();
            System.out.flush();
        }
    }

    protected void printRequestStatus(final CharSequence what, final CharSequence status, final CharSequence comment) {
        synchronized (System.out) {
            printRequestStatusHelper(what, status).append(' ').append(comment).println();
            System.out.flush();
        }
    }

    protected void buildRequestIdString() {
        Util.printInt(Util.printInt(idString.clear().put(LITERAL_REQUEST), threadId).put(','), requestId).put(')');
        Util.rewindBuffer(idString);
    }

    protected void buildRequestString(final String prefix) {
        Util.printInt(Util.printInt(requestString.clear().put(prefix), threadId).put('_'), requestId);
        Util.rewindBuffer(requestString);
    }
}

abstract class ClientProgram {
    protected static final int SO_TIMEOUT = 100;

    protected InetSocketAddress resolveAddress(final String host, final int port) {
        final InetSocketAddress address = new InetSocketAddress(host, port);
        if (address.isUnresolved()) {
            throw new HelloUDPError("Could not resolve host's IP address");
        }
        return address;
    }

    protected static void main(final Supplier<HelloClient> factory, final String[] args) {
        final String usage = "java <client> <remote host address> <remote host port> " +
                "<request prefix> <number of parallel threads> <number of requests per thread>";
        if (args.length < 5) {
            System.err.println("Insufficient arguments");
            System.out.println(usage);
        }
        try {
            factory.get().run(args[0], Integer.parseInt(args[1]), args[2], Integer.parseInt(args[3]), Integer.parseInt(args[4]));
        } catch (final NumberFormatException exc) {
            System.out.println("Wrong argument number format: " + exc.toString());
        } catch (final HelloUDPError exc) {
            System.out.println(exc.toString());
            exc.printStackTrace();
        }
    }
}

abstract class ServerProgram implements AutoCloseable {
    protected ExecutorService executorService;
    protected Thread dealer;
    protected final static int QUEUE_SIZE = 1000;
    protected static final String RESPONSE_PREFIX = "Hello, ";

    String transformRequest(final String request) {
        return RESPONSE_PREFIX + request;
    }

    void start(final int threads, final Runnable runnable) {
        executorService = new ThreadPoolExecutor(1, threads, 30, TimeUnit.SECONDS,
                new ArrayBlockingQueue<>(QUEUE_SIZE), (r, executor) -> { /* ignore */ });
        dealer = new Thread(runnable);
        dealer.start();
    }

    @Override
    public void close() {
        boolean interrupted = Thread.interrupted();
        while (dealer.isAlive()) {
            try {
                dealer.join();
            } catch (final InterruptedException e) {
                interrupted = true;
            }
        }
        if (interrupted) {
            Thread.currentThread().interrupt();
        }
        Util.closeExecutorService(executorService);
    }

    public static void main(final Supplier<HelloServer> factory, final String[] args) {
        final String usage = "<server> <local port> <number of parallel threads>";
        if (args.length < 2) {
            System.err.println("Insufficient arguments");
            System.out.println(usage);
        }
        try (final HelloServer server = factory.get()) {
            server.start(Integer.parseInt(args[0]), Integer.parseInt(args[1]));
            System.out.println("Press return key to exit the server...");
            System.in.read();
        } catch (final NumberFormatException exc) {
            System.out.println("Wrong argument number format: " + exc.toString());
        } catch (final HelloUDPError | IOException exc) {
            System.out.println(exc.toString());
            exc.printStackTrace();
        }
    }
}