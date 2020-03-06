package expression;

import expression.operation.OperableTable;

import java.util.Map;

public interface CommonExpression extends BaseExpression, TripleExpression {
    @Override
    default <T extends OperableTable<T, EvalT>, EvalT extends Number> EvalT evaluate(T table, EvalT x, EvalT y, EvalT z) {
        return evaluate(table, Map.of("x", x, "y", y, "z", z));
    }
}
