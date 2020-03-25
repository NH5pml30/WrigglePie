package expression.operation;

import expression.CommonExpression;

import java.util.Map;

public class Count<T extends OperableTable<T, EvalT>, EvalT extends Number>
        extends UnaryOperation<T, EvalT> {
    private static UnaryOperationTableEntry entry;
    public static void setEntry(UnaryOperationTableEntry newEntry) {
        entry = newEntry;
    }
    public static UnaryOperationTableEntry getEntry() {
        return entry;
    }

    public Count(final T table,
                 final CommonExpression<T, EvalT> expr
    ) {
        super(table, entry, expr);
    }

    @Override
    EvalT evaluateUnsafe(final Map<String, EvalT> x) {
        return evaluateHelper(table::count, x);
    }
}
