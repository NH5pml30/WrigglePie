package expression.exception;

public class BinaryOverflowException extends BinaryOperationException {
    public BinaryOverflowException(Number left, Number right) {
        super(left, right, "overflow");
    }
}
