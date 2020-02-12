package expression.exception;

public class PowException extends BinaryOperationException {
    public PowException( Number left, Number right ) {
        super(left, right, "exponent < 0 or 0 to the power 0");
    }
}
