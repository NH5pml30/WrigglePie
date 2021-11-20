package info.kgeorgiy.ja.holyavin;

public class ChatError extends RuntimeException {
    public ChatError(final String message) {
        super(message);
    }

    public ChatError(final String message, final Throwable cause) {
        super(message, cause);
    }
}
