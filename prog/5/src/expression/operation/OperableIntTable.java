package expression.operation;

import expression.exception.*;

public final class OperableIntTable implements OperableTable<OperableIntTable, Integer> {
    private static final OperableIntTable instance = new OperableIntTable();

    public static OperableIntTable getInstance() {
        return instance;
    }

    @Override
    public Integer parseNumber(String str) {
        try {
            return Integer.parseInt(str);
        } catch (NumberFormatException e) {
            throw new ConstFormatException("signed 32-bit integer", str);
        }
    }

    @Override
    public Integer add(Integer left, Integer right) {
        if (right > 0 && left > Integer.MAX_VALUE - right) {
            throw new BinaryOverflowException(left, right);
        } else if (right < 0 && left < Integer.MIN_VALUE - right) {
            throw new BinaryUnderflowException(left, right);
        }
        return left + right;
    }

    @Override
    public Integer subtract(Integer left, Integer right) {
        if (right < 0 && left > Integer.MAX_VALUE + right) {
            throw new BinaryOverflowException(left, right);
        } else if (right > 0 && left < Integer.MIN_VALUE + right) {
            throw new BinaryUnderflowException(left, right);
        }
        return left - right;
    }

    @Override
    public Integer divide(Integer left, Integer right) {
        if (right == 0) {
            throw new ZeroDivisionException(left);
        } else if (left == Integer.MIN_VALUE && right == -1) {
            throw new BinaryOverflowException(left, right);
        }
        return left / right;
    }

    @Override
    public Integer multiply(Integer left, Integer right) {
        int res = left * right;
        if (left == -1 && right == Integer.MIN_VALUE ||
                left != 0 && res / left != right) {
            if ((left > 0) != (right > 0)) {
                throw new BinaryUnderflowException(left, right);
            } else {
                throw new BinaryOverflowException(left, right);
            }
        }
        return res;
    }

    @Override
    public Integer negate(Integer val) {
        int neg = -val;
        if ((val > 0) != (neg < 0)) {
            throw new UnaryOverflowException(val);
        }
        return neg;
    }

    @Override
    public Integer pow(Integer base, Integer exp) {
        if (exp < 0) {
            throw new PowException(base, exp, "negative exponent (makes no sense in integers)");
        } else if (base == 0 && exp == 0) {
            throw new PowException(base, exp, "zero to the power zero (undefined)");
        }

        int x = base, n = exp;
        try {
            int r = 1;
            while (n != 0) {
                if (n % 2 == 1) {
                    r = multiply(r, x);
                }
                n /= 2;
                if (n != 0) {
                    x = multiply(x, x);
                }
            }
            return r;
        } catch (BinaryOverflowException e) {
            throw new BinaryOverflowException(base, exp);
        }
    }

    @Override
    public Integer log(Integer x, Integer base) {
        if (base <= 1) {
            throw new LogException(x, base, "base less or equal to 1");
        } else if (x <= 0) {
            throw new LogException(x, base, "non-positive argument");
        }

        int res = 0;
        while (x >= base) {
            x /= base;
            res++;
        }
        return res;
    }

    @Override
    public Integer log2(Integer x) {
        if (x <= 0) {
            throw new Log2NegException(x);
        }
        return 31 - Integer.numberOfLeadingZeros(x);
    }

    @Override
    public Integer pow2(Integer x) {
        if (x >= 32) {
            throw new UnaryOverflowException(x);
        } else if (x < 0) {
            throw new Pow2NegException(x);
        }
        return 1 << x;
    }
}
