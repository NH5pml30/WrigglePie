package expression.operation;

import expression.CommonExpression;

public class CheckedLog extends BinaryOperation {
    static public final BinaryOperationTableEntry entry = BinaryOperationTableEntry.LOG;

    public CheckedLog( CommonExpression left, CommonExpression right ) {
        super(OperableInt::log, entry, left, right);
    }
}
