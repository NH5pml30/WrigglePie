package expression.operation.exception;

public class PowException extends BinaryOperationException {
    public PowException(Number left, Number right, String message) {
        super(left, right, "exponent domain violation: " + message);
    }
}
