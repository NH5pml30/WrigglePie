package expression;

public class BinaryOverflowException extends BinaryOperationException {
    public BinaryOverflowException( Number left, Number right, String op ) {
        super(left, right, op, "overflow");
    }
}
