package expression;

import expression.operation.OperableTable;

import java.util.Map;

public interface BaseExpression extends ToMiniString {
    <T extends OperableTable<T, EvalT>, EvalT extends Number> EvalT evaluate(T table, Map<String, EvalT> x);
}
