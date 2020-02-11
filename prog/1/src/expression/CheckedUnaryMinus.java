package expression;

public class CheckedUnaryMinus extends UnaryOperation {
    public static final UnaryOperationTableEntry entry = UnaryOperationTableEntry.UNARY_MINUS;

    public CheckedUnaryMinus(CommonExpression expr ) {
        super(OperableInt::negate, entry, expr);
    }
}
