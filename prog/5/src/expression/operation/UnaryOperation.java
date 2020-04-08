package expression.operation;

import expression.CommonExpression;

import java.util.Map;
import java.util.function.UnaryOperator;

public abstract class UnaryOperation<T extends OperableTable<T, EvalT>, EvalT extends Number>
        extends OperationBase<T, EvalT> {
    private final UnaryOperationTableEntry entry;
    private final CommonExpression<T, EvalT> expr;

    public interface Factory {
        <T extends OperableTable<T, EvalT>,
         EvalT extends Number>
        UnaryOperation<T, EvalT> create(
            final T table,
            final CommonExpression<T, EvalT> expr
        );
    }

    UnaryOperation(final T table,
                   final UnaryOperationTableEntry entry,
                   final CommonExpression<T, EvalT> expr) {
        super(table, entry);
        this.entry = entry;
        this.expr = expr;
    }

    EvalT evaluateHelper(final UnaryOperator<EvalT> op, final Map<String, EvalT> x) {
        return op.apply(expr.evaluate(x));
    }

    @Override
    public String toString() {
        return entry.toString() + "(" + expr.toString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        UnaryOperation<?, ?> op = (UnaryOperation<?, ?>)o;
        return op.table == table && op.entry == entry && expr.equals(op.expr);
    }

    @Override
    public int hashCode() {
        return (expr.hashCode() * 9343 + entry.hashCode() * 31) * 31;
    }
}
