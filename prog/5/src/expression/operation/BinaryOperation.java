package expression.operation;

import expression.CommonExpression;

import java.util.Map;
import java.util.function.BinaryOperator;

public abstract class BinaryOperation
    extends OperationBase {
    private final BinaryOperationTableEntry entry;
    protected final CommonExpression left, right;

    BinaryOperation(final BinaryOperationTableEntry entry,
                    final CommonExpression left, final CommonExpression right) {
        super(entry);
        this.entry = entry;
        this.left = left;
        this.right = right;
    }

    protected <T extends OperableTable<T, EvalT>, EvalT extends Number>
    EvalT evaluateHelper(T table, final BinaryOperator<EvalT> op, Map<String, EvalT> x) {
        return op.apply(left.evaluate(table, x), right.evaluate(table, x));
    }

    @Override
    public String toString() {
        return '(' + left.toString() +
                   ' ' + entry.toString() + ' ' +
                   right.toString() + ')';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        BinaryOperation op = (BinaryOperation)o;
        return left.equals(op.left) && right.equals(op.right);
    }

    @Override
    public int hashCode() {
        return (left.hashCode() * 9343 + entry.hashCode() * 31) * 7919 + right.hashCode() * 31;
    }
}
