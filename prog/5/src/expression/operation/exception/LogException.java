package expression.operation.exception;

public class LogException extends BinaryOperationException {
    public LogException(Number left, Number right, String message) {
        super(left, right, "logarithm function domain violation: " + message);
    }
}
