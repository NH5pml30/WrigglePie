package expression.exception;

public class UnaryOverflowException extends UnaryOperationException {
    public UnaryOverflowException( Number val ) {
        super(val, "overflow");
    }
}
