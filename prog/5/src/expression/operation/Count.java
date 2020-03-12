package expression.operation;

import expression.CommonExpression;

import java.util.Map;

public class Count extends UnaryOperation {
    static public final UnaryOperationTableEntry entry = UnaryOperationTableEntry.COUNT;

    public Count(CommonExpression expr) {
        super(entry, expr);
    }

    @Override
    <T extends OperableTable<T, EvalT>, EvalT extends Number> EvalT
    evaluateUnsafe(T table, Map<String, EvalT> x) {
        return evaluateHelper(table, table::count, x);
    }
}
