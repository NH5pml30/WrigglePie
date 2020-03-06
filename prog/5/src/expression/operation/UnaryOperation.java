package expression.operation;

import expression.CommonExpression;

import java.util.Map;
import java.util.function.UnaryOperator;

public abstract class UnaryOperation extends OperationBase {
    private final UnaryOperationTableEntry entry;
    private final CommonExpression expr;

    UnaryOperation(final UnaryOperationTableEntry entry,
                   final CommonExpression expr) {
        super(entry);
        this.entry = entry;
        this.expr = expr;
    }

    protected <T extends OperableTable<T, EvalT>, EvalT extends Number>
    EvalT evaluateHelper(T table, final UnaryOperator<EvalT> op, Map<String, EvalT> x) {
        return op.apply(expr.evaluate(table, x));
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
        UnaryOperation op = (UnaryOperation)o;
        return expr.equals(op.expr);
    }

    @Override
    public int hashCode() {
        return (expr.hashCode() * 9343 + entry.hashCode() * 31) * 31;
    }
}
