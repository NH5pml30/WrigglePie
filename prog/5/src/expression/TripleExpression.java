package expression;

import expression.operation.*;

import java.math.BigInteger;
import java.util.Map;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface TripleExpression extends ToMiniString {
    int evaluate(int x, int y, int z);

    short evaluate(short x, short y, short z);

    long evaluate(long x, long y, long z);

    double evaluate(double x, double y, double z);

    BigInteger evaluate(BigInteger x, BigInteger y, BigInteger z);
}
