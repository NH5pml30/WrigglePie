package expression.operation;

import expression.CommonExpression;

import java.util.Map;

public class Log2 extends UnaryOperation {
    private static UnaryOperationTableEntry entry;
    public static void setEntry(UnaryOperationTableEntry newEntry) {
        entry = newEntry;
    }
    public static UnaryOperationTableEntry getEntry() {
        return entry;
    }

    public Log2(CommonExpression expr) {
        super(entry, expr);
    }

    @Override
    <T extends OperableTable<T, EvalT>, EvalT extends Number> EvalT
    evaluateUnsafe(T table, Map<String, EvalT> x) {
        return evaluateHelper(table, table::log2, x);
    }
}
