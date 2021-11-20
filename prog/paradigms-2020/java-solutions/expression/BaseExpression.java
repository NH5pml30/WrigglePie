package expression;

import expression.operation.OperableTable;

import java.util.Map;

public interface BaseExpression<T extends OperableTable<T, EvalT>, EvalT extends Number>
        extends ToMiniString {
    EvalT evaluate(final Map<String, EvalT> x);
}
