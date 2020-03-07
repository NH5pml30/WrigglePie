package expression.operation;

import expression.CommonExpression;
import expression.operation.exception.EvaluationException;

import java.util.Map;

public abstract class OperationBase
    implements Comparable<OperationBase>, CommonExpression {
    final private OperationTableBase entry;

    OperationBase(OperationTableBase entry) {
        this.entry = entry;
    }

    abstract <T extends OperableTable<T, EvalT>, EvalT extends Number>
    EvalT evaluateUnsafe(T table, Map<String, EvalT> x);

    @Override
    public <T extends OperableTable<T, EvalT>, EvalT extends Number>
    EvalT evaluate(T table, Map<String, EvalT> x) {
        try {
            return evaluateUnsafe(table, x);
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
    public int compareTo(OperationBase other) {
        return entry.comparePrior(other.entry);
    }
}
