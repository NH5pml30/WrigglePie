package expression.operation;

import expression.operation.exception.LogException;
import expression.operation.exception.PowException;
import expression.operation.exception.ZeroDivisionException;

import java.math.BigInteger;

public final class OperableBigIntTable implements OperableTable<OperableBigIntTable, BigInteger> {
    private static final OperableBigIntTable instance = new OperableBigIntTable();

    public static OperableBigIntTable getInstance() {
        return instance;
    }

    @Override
    public BigInteger parseNumber(String str) {
        return new BigInteger(str);
    }

    @Override
    public BigInteger add(BigInteger left, BigInteger right) {
        return left.add(right);
    }

    @Override
    public BigInteger subtract(BigInteger left, BigInteger right) {
        return left.subtract(right);
    }

    @Override
    public BigInteger divide(BigInteger left, BigInteger right) {
        if (right.equals(BigInteger.ZERO)) {
            throw new ZeroDivisionException(left);
        }
        return left.divide(right);
    }

    @Override
    public BigInteger multiply(BigInteger left, BigInteger right) {
        return left.multiply(right);
    }

    @Override
    public BigInteger negate(BigInteger val) {
        return val.negate();
    }

    @Override
    public BigInteger min(BigInteger left, BigInteger right) {
        return left.min(right);
    }

    @Override
    public BigInteger max(BigInteger left, BigInteger right) {
        return left.max(right);
    }

    @Override
    public BigInteger count(BigInteger x) {
        return BigInteger.valueOf(x.bitCount());
    }
}
