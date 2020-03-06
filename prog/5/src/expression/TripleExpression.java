package expression;

import expression.operation.OperableTable;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface TripleExpression extends ToMiniString {
    <T extends OperableTable<T, EvalT>, EvalT extends Number> EvalT evaluate(T table, EvalT x, EvalT y, EvalT z);
}
