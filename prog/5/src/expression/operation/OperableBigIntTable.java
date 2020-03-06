package expression.operation;

import expression.exception.LogException;
import expression.exception.PowException;
import expression.exception.ZeroDivisionException;

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
    public BigInteger pow(BigInteger base, BigInteger exp) {
        if (exp.compareTo(BigInteger.ZERO) < 0) {
            throw new PowException(base, exp, "negative exponent (makes no sense in integers)");
        } else if (base.compareTo(BigInteger.ZERO) == 0 && exp.compareTo(BigInteger.ZERO) == 0) {
            throw new PowException(base, exp, "zero to the power zero (undefined)");
        }

        BigInteger x = base, n = exp;
        BigInteger r = BigInteger.ONE;
        while (n.compareTo(BigInteger.ZERO) != 0) {
            if (n.mod(BigInteger.TWO).compareTo(BigInteger.ONE) == 0) {
                r = multiply(r, x);
            }
            n = n.divide(BigInteger.TWO);
            if (n.compareTo(BigInteger.ZERO) != 0) {
                x = multiply(x, x);
            }
        }
        return r;
    }

    @Override
    public BigInteger log(BigInteger x, BigInteger base) {
        if (base.compareTo(BigInteger.ONE) <= 0) {
            throw new LogException(x, base, "base less or equal to 1");
        } else if (x.compareTo(BigInteger.ZERO) <= 0) {
            throw new LogException(x, base, "non-positive argument");
        }

        BigInteger res = BigInteger.ZERO;
        while (x.compareTo(base) >= 0) {
            x = divide(x, base);
            res = res.add(BigInteger.ONE);
        }
        return res;
    }

    @Override
    public BigInteger log2(BigInteger x) {
        return log(x, BigInteger.TWO);
    }

    @Override
    public BigInteger pow2(BigInteger x) {
        return pow(BigInteger.TWO, x);
    }
}
