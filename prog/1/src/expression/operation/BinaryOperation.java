package expression.operation;

import expression.CommonExpression;

import java.util.Map;
import java.util.function.BinaryOperator;

public abstract class BinaryOperation extends OperationBase implements CommonExpression {
    private final BinaryOperator<Integer> operator;
    private final BinaryOperationTableEntry entry;
    private final CommonExpression left, right;

    BinaryOperation( final BinaryOperator<Integer> operator, final BinaryOperationTableEntry entry,
                     final CommonExpression left, final CommonExpression right ) {
        super(entry);
        this.entry = entry;
        this.operator = operator;
        this.left = left;
        this.right = right;
    }

    @Override
    public int evaluateUnsafe( Map<String, Integer> x ) {
        return operator.apply(left.evaluate(x), right.evaluate(x));
    }

    @Override
    public String toString() {
        return '(' + left.toString() +
                   ' ' + entry.toString() + ' ' +
                   right.toString() + ')';
    }

    @Override
    public boolean equals( Object o ) {
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
