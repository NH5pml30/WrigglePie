package expression;

import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;

public abstract class OperationBase implements Comparable<OperationBase>, CommonExpression {
    final private OperationTableBase entry;

    OperationBase( OperationTableBase entry ) {
        this.entry = entry;
    }

    abstract int evaluateUnsafe( Map<String, Integer> x );

    @Override
    public int evaluate( Map<String, Integer> x ) {
        try {
            return evaluateUnsafe(x);
        } catch ( EvaluationException e ) {
            StringBuilder sb = new StringBuilder(" = ").append(toString()).append(" (with ");
            boolean isFirst = true;
            for (Map.Entry<String, Integer> entry : x.entrySet()) {
                if (isFirst) {
                    isFirst = false;
                } else {
                    sb.append(", ");
                }
                sb.append(entry.getKey()).append(" = ").append(entry.getValue());
            }
            sb.append(")");
            e.addExpr(sb.toString());
            throw e;
        }
    }

    @Override
    public int compareTo( OperationBase other ) {
        return entry.comparePrior(other.entry);
    }
}
