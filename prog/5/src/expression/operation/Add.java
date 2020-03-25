package expression.operation;

import expression.CommonExpression;

import java.util.Map;

public class Add<T extends OperableTable<T, EvalT>, EvalT extends Number>
        extends BinaryOperation<T, EvalT> {
    private static BinaryOperationTableEntry entry;
    public static void setEntry(BinaryOperationTableEntry newEntry) {
        entry = newEntry;
    }
    public static BinaryOperationTableEntry getEntry() {
        return entry;
    }

    public Add(final T table,
               final CommonExpression<T, EvalT> left,
               final CommonExpression<T, EvalT> right
    ) {
        super(table, entry, left, right);
    }

    @Override
    EvalT evaluateUnsafe(final Map<String, EvalT> x) {
        return evaluateHelper(table::add, x);
    }
}
