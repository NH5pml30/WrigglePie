package expression.operation;

import expression.CommonExpression;

public class CheckedDivide extends BinaryOperation {
    static public final BinaryOperationTableEntry entry = BinaryOperationTableEntry.DIVIDE;

    public CheckedDivide( CommonExpression left, CommonExpression right ) {
        super(OperableInt::divide, entry, left, right);
    }
}
