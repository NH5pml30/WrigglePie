package expression.operation;

import expression.CommonExpression;

import java.util.Map;

public class Divide extends BinaryOperation {
    static public final BinaryOperationTableEntry entry = BinaryOperationTableEntry.DIVIDE;

    public Divide(CommonExpression left, CommonExpression right) {
        super(entry, left, right);
    }

    @Override
    <T extends OperableTable<T, EvalT>, EvalT extends Number> EvalT
    evaluateUnsafe(T table, Map<String, EvalT> x) {
        return evaluateHelper(table, table::divide, x);
    }
}
