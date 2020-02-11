package expression;

public class CheckedSubtract extends BinaryOperation {
    static public final BinaryOperationTableEntry entry = BinaryOperationTableEntry.SUBTRACT;

    public CheckedSubtract(CommonExpression left, CommonExpression right ) {
        super(OperableInt::subtract, entry, left, right);
    }
}
