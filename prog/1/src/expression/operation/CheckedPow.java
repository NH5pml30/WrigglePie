package expression.operation;

import expression.CommonExpression;

public class CheckedPow extends BinaryOperation {
    static public final BinaryOperationTableEntry entry = BinaryOperationTableEntry.POW;

    public CheckedPow( CommonExpression left, CommonExpression right ) {
        super(OperableInt::pow, entry, left, right);
    }
}
