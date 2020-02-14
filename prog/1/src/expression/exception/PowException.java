package expression.exception;

public class PowException extends BinaryOperationException {
    public PowException( Number left, Number right ) {
        super(left, right, "negative exponent or 0 to the power 0");
    }
}
