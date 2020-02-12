package expression.operation;

import expression.CommonExpression;

public class CheckedLog2 extends UnaryOperation {
    public static final UnaryOperationTableEntry entry = UnaryOperationTableEntry.LOG2;

    public CheckedLog2( CommonExpression expr ) {
        super(OperableInt::log2, entry, expr);
    }
}
