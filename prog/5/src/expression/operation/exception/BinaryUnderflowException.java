package expression.operation.exception;

public class BinaryUnderflowException extends BinaryOperationException {
    public BinaryUnderflowException(Number left, Number right) {
        super(left, right, "underflow");
    }
}
