package expression.operation;

import expression.CommonExpression;

import java.util.Map;

public class Multiply extends BinaryOperation {
    static public final BinaryOperationTableEntry entry = BinaryOperationTableEntry.MULTIPLY;

    public Multiply(CommonExpression left, CommonExpression right) {
        super(entry, left, right);
    }

    @Override
    <T extends OperableTable<T, EvalT>, EvalT extends Number> EvalT
    evaluateUnsafe(T table, Map<String, EvalT> x) {
        return evaluateHelper(table, table::multiply, x);
    }
}
