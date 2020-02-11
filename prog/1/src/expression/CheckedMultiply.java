package expression;

public class CheckedMultiply extends BinaryOperation {
    static public final BinaryOperationTableEntry entry = BinaryOperationTableEntry.MULTIPLY;

    public CheckedMultiply(CommonExpression left, CommonExpression right ) {
        super(OperableInt::multiply, entry, left, right);
    }
}
