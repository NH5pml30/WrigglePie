package expression.operation.exception;

public class Log2NegException extends UnaryOperationException {
    public Log2NegException(Number val) {
        super(val, "log2 of non-positive argument");
    }
}
