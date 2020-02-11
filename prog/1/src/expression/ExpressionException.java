package expression;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 * @author Nikolai Kholiavin
 */
public class ExpressionException extends RuntimeException {
    public ExpressionException( final String message ) {
        super(message);
    }

    public ExpressionException( final String message, Throwable cause ) {
        super(message, cause);
    }
}
