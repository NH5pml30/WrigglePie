package expression;

public class OperableInt {
    private OperableInt() {
    }

    public static int add( int left, int right ) {
        if (right > 0 && left > Integer.MAX_VALUE - right ||
            right < 0 && left < Integer.MIN_VALUE - right) {
            throw new BinaryOverflowException(left, right, "+");
        }
        return left + right;
    }

    public static int subtract( int left, int right ) {
        if (right < 0 && left > Integer.MAX_VALUE + right ||
            right > 0 && left < Integer.MIN_VALUE + right) {
            throw new BinaryOverflowException(left, right, "-");
        }
        return left - right;
    }

    public static int divide( int left, int right ) {
        if (right == 0) {
            throw new ZeroDivisionException(left);
        } else if (left == Integer.MIN_VALUE && right == -1) {
            throw new BinaryOverflowException(left, right, "/");
        }
        return left / right;
    }

    public static int multiply( int left, int right ) {
        int a1 = left >> 16, a0 = left & 0xFFFF, b1 = right >> 16, b0 = right & 0xFFFF;
        if (a1 > 0 && b1 > 0) {
            throw new BinaryOverflowException(left, right, "*");
        }
        int high = a1 * b0 + a0 * b1;
        if (high > Short.MAX_VALUE) {
            throw new BinaryOverflowException(left, right, "*");
        }
        return (high << 16) + a0 * b0;
    }

    public static int negate( int val ) {
        int neg = -val;
        if (neg < 0) {
            throw new UnaryOverflowException(val, "-");
        }
        return neg;
    }
}
