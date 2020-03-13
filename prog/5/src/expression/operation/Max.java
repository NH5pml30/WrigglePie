package expression.operation;

import expression.CommonExpression;

import java.util.Map;

public class Max extends BinaryOperation {
    private static BinaryOperationTableEntry entry;
    public static void setEntry(BinaryOperationTableEntry newEntry) {
        entry = newEntry;
    }
    public static BinaryOperationTableEntry getEntry() {
        return entry;
    }

    public Max(CommonExpression left, CommonExpression right) {
        super(entry, left, right);
    }

    @Override
    <T extends OperableTable<T, EvalT>, EvalT extends Number> EvalT
    evaluateUnsafe(T table, Map<String, EvalT> x) {
        return evaluateHelper(table, table::max, x);
    }
}
