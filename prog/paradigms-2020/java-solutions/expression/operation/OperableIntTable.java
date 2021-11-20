package expression.operation;

import expression.operation.exception.*;
import expression.operation.exception.ConstFormatException;
import expression.parser.exception.ParserException;

public final class OperableIntTable implements OperableTable<OperableIntTable, Integer> {
    private static final OperableIntTable instance = new OperableIntTable();

    public static OperableIntTable getInstance() {
        return instance;
    }

    @Override
    public Integer parseNumber(String str) throws ConstFormatException {
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
    public Integer min(Integer left, Integer right) {
        return Integer.min(left, right);
    }

    @Override
    public Integer max(Integer left, Integer right) {
        return Integer.max(left, right);
    }

    @Override
    public Integer count(Integer x) {
        return Integer.bitCount(x);
    }
}
