package expression.operation;

import expression.exception.*;

public final class OperableInt {
    private OperableInt() {
    }

    public static int add( int left, int right ) {
        if (right > 0 && left > Integer.MAX_VALUE - right) {
            throw new BinaryOverflowException(left, right);
        } else if (right < 0 && left < Integer.MIN_VALUE - right) {
            throw new BinaryUnderflowException(left, right);
        }
        return left + right;
    }

    public static int subtract( int left, int right ) {
        if (right < 0 && left > Integer.MAX_VALUE + right) {
            throw new BinaryOverflowException(left, right);
        } else if (right > 0 && left < Integer.MIN_VALUE + right) {
            throw new BinaryUnderflowException(left, right);
        }
        return left - right;
    }

    public static int divide( int left, int right ) {
        if (right == 0) {
            throw new ZeroDivisionException(left);
        } else if (left == Integer.MIN_VALUE && right == -1) {
            throw new BinaryOverflowException(left, right);
        }
        return left / right;
    }

    public static int multiply( int left, int right ) {
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

    public static int negate( int val ) {
        int neg = -val;
        if ((val > 0) != (neg < 0)) {
            throw new UnaryOverflowException(val);
        }
        return neg;
    }

    public static int pow( int base, int exp ) {
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
        } catch ( BinaryOverflowException e ) {
            throw new BinaryOverflowException(base, exp);
        }
    }

    public static int log( int x, int base ) {
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

    public static int log2( int x ) {
        if (x <= 0) {
            throw new Log2NegException(x);
        }
        return 31 - Integer.numberOfLeadingZeros(x);
    }

    public static int pow2( int x ) {
        if (x >= 32) {
            throw new UnaryOverflowException(x);
        } else if (x < 0) {
            throw new Pow2NegException(x);
        }
        return 1 << x;
    }
}
