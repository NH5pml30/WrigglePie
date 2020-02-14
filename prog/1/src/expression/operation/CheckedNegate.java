package expression.operation;

import expression.CommonExpression;

public class CheckedNegate extends UnaryOperation {
    public static final UnaryOperationTableEntry entry = UnaryOperationTableEntry.UNARY_MINUS;

    public CheckedNegate(CommonExpression expr ) {
        super(OperableInt::negate, entry, expr);
    }
}
