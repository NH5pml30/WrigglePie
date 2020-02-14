package expression.exception;

public class LogException extends BinaryOperationException {
    public LogException( Number left, Number right ) {
        super(left, right, "logarithm with non-positive base, base 1 or with non-positive argument");
    }
}
