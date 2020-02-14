package expression.operation;

import expression.CommonExpression;

public class CheckedPow2 extends UnaryOperation {
    public static final UnaryOperationTableEntry entry = UnaryOperationTableEntry.POW2;

    public CheckedPow2( CommonExpression expr ) {
        super(OperableInt::pow2, entry, expr);
    }
}
