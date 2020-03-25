package expression.operation;

import expression.CommonExpression;

import java.util.Map;
import java.util.function.BinaryOperator;

public abstract class BinaryOperation<T extends OperableTable<T, EvalT>, EvalT extends Number>
    extends OperationBase<T, EvalT> {
    public interface Factory {
        <T extends OperableTable<T, EvalT>,
         EvalT extends Number>
        BinaryOperation<T, EvalT> create(
            final T table,
            final CommonExpression<T, EvalT> left,
            final CommonExpression<T, EvalT> right
        );
    }

    private final BinaryOperationTableEntry entry;
    protected final CommonExpression<T, EvalT> left, right;

    BinaryOperation(final T table, final BinaryOperationTableEntry entry,
                    final CommonExpression<T, EvalT> left, final CommonExpression<T, EvalT> right) {
        super(table, entry);
        this.entry = entry;
        this.left = left;
        this.right = right;
    }

    EvalT evaluateHelper(final BinaryOperator<EvalT> op, final Map<String, EvalT> x) {
        return op.apply(left.evaluate(x), right.evaluate(x));
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
        BinaryOperation<T, EvalT> op = (BinaryOperation<T, EvalT>)o;
        return left.equals(op.left) && right.equals(op.right);
    }

    @Override
    public int hashCode() {
        return (left.hashCode() * 9343 + entry.hashCode() * 31) * 7919 + right.hashCode() * 31;
    }
}
