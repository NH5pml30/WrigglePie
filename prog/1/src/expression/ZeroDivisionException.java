package expression;

public class ZeroDivisionException extends BinaryOperationException {
    public ZeroDivisionException( Number left ) {
        super(left, 0, "/", "division by zero");
    }
}
