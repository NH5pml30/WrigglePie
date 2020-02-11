package expression;

public class UnaryOverflowException extends UnaryOperationException {
    public UnaryOverflowException( Number val, String op ) {
        super(val, op, "overflow");
    }
}
