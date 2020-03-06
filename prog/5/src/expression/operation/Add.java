package expression.operation;

import expression.CommonExpression;

import java.util.Map;

public class Add extends BinaryOperation {
    static public final BinaryOperationTableEntry entry = BinaryOperationTableEntry.ADD;

    public Add(CommonExpression left, CommonExpression right) {
        super(entry, left, right);
    }

    @Override
    <T extends OperableTable<T, EvalT>, EvalT extends Number> EvalT evaluateUnsafe(T table, Map<String, EvalT> x) {
        return evaluateHelper(table, table::add, x);
    }
}
