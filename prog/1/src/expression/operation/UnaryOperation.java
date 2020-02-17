package expression.operation;

import expression.CommonExpression;

import java.util.Map;
import java.util.function.UnaryOperator;

public abstract class UnaryOperation extends OperationBase implements CommonExpression {
    private final UnaryOperator<Integer> operator;
    private final UnaryOperationTableEntry entry;
    private final CommonExpression expr;

    UnaryOperation( final UnaryOperator<Integer> operator, final UnaryOperationTableEntry entry,
                    final CommonExpression expr ) {
        super(entry);
        this.entry = entry;
        this.operator = operator;
        this.expr = expr;
    }

    @Override
    public int evaluateUnsafe( Map<String, Integer> x ) {
        return operator.apply(expr.evaluate(x));
    }

    @Override
    public String toString() {
        return entry.toString() + "(" + expr.toString() + ")";
    }

    @Override
    public boolean equals( Object o ) {
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
