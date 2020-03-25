package expression.operation;

import expression.CommonExpression;
import expression.operation.exception.EvaluationException;

import java.util.Map;

public abstract class OperationBase<T extends OperableTable<T, EvalT>, EvalT extends Number>
    implements
        Comparable<OperationBase<T, EvalT>>,
        CommonExpression<T, EvalT> {
    final private OperationTableBase entry;
    final protected OperableTable<T, EvalT> table;

    OperationBase(final T table, final OperationTableBase entry) {
        this.table = table;
        this.entry = entry;
    }

    abstract EvalT evaluateUnsafe(final Map<String, EvalT> x);

    @Override
    public EvalT evaluate(final Map<String, EvalT> x) {
        try {
            return evaluateUnsafe(x);
        } catch (EvaluationException e) {
            if (e.getFilled()) {
                throw e;
            }

            e.setOp(entry.toString());
            StringBuilder sb = new StringBuilder(" = ").append(toString()).append(" (with ");
            boolean isFirst = true;
            for (Map.Entry<String, EvalT> entry : x.entrySet()) {
                if (isFirst) {
                    isFirst = false;
                } else {
                    sb.append(", ");
                }
                sb.append(entry.getKey()).append(" = ").append(entry.getValue());
            }
            sb.append(")");
            e.addExpr(sb.toString());
            e.setFilled();
            throw e;
        }
    }

    @Override
    public int compareTo(final OperationBase<T, EvalT> other) {
        return entry.comparePrior(other.entry);
    }
}
