package expression;

import expression.operation.*;

import java.math.BigInteger;
import java.util.Map;

public interface CommonExpression extends BaseExpression, TripleExpression {
    default <T extends OperableTable<T, EvalT>, EvalT extends Number> EvalT evaluate(T table, EvalT x, EvalT y, EvalT z) {
        return evaluate(table, Map.of("x", x, "y", y, "z", z));
    }

    @Override
    default int evaluate(int x, int y, int z) {
        return evaluate(OperableIntTable.getInstance(), Map.of("x", x, "y", y, "z", z));
    }

    @Override
    default short evaluate(short x, short y, short z) {
        return evaluate(OperableShortTable.getInstance(), Map.of("x", x, "y", y, "z", z));
    }

    @Override
    default long evaluate(long x, long y, long z) {
        return evaluate(OperableLongTable.getInstance(), Map.of("x", x, "y", y, "z", z));
    }

    @Override
    default double evaluate(double x, double y, double z) {
        return evaluate(OperableDoubleTable.getInstance(), Map.of("x", x, "y", y, "z", z));
    }

    @Override
    default BigInteger evaluate(BigInteger x, BigInteger y, BigInteger z) {
        return evaluate(OperableBigIntTable.getInstance(), Map.of("x", x, "y", y, "z", z));
    }
}
