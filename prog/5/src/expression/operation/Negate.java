package expression.operation;

import expression.CommonExpression;

import java.util.Map;

public class Negate extends UnaryOperation {
    public static final UnaryOperationTableEntry entry = UnaryOperationTableEntry.UNARY_MINUS;

    public Negate(CommonExpression expr) {
        super(entry, expr);
    }

    @Override
    <T extends OperableTable<T, EvalT>, EvalT extends Number> EvalT
    evaluateUnsafe(T table, Map<String, EvalT> x) {
        return evaluateHelper(table, table::negate, x);
    }
}
