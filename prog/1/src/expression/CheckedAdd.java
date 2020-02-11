package expression;

public class CheckedAdd extends BinaryOperation {
    static public final BinaryOperationTableEntry entry = BinaryOperationTableEntry.ADD;

    public CheckedAdd(CommonExpression left, CommonExpression right ) {
        super(OperableInt::add, entry, left, right);
    }
}
