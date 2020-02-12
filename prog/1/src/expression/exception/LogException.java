package expression.exception;

public class LogException extends BinaryOperationException {
    public LogException( Number left, Number right ) {
        super(left, right, "logarithm with non-positive base or base 1 or of non-positive argument");
    }
}
