package expression;

import expression.operation.*;

import java.math.BigInteger;
import java.util.Map;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface TripleExpression<T extends OperableTable<T, EvalT>, EvalT extends Number>
        extends ToMiniString {
    EvalT evaluate(EvalT x, EvalT y, EvalT z);
}
