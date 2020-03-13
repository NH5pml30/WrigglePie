package expression.operation;

import expression.operation.exception.ZeroDivisionException;

public final class OperableShortTable implements OperableTable<OperableShortTable, Short> {
    private static final OperableShortTable instance = new OperableShortTable();

    public static OperableShortTable getInstance() {
        return instance;
    }

    @Override
    public Short parseNumber(String str) {
        return Short.parseShort(str);
    }

    @Override
    public Short add(Short left, Short right) {
        return (short)(left + right);
    }

    @Override
    public Short subtract(Short left, Short right) {
        return (short)(left - right);
    }

    @Override
    public Short divide(Short left, Short right) {
        if (right == 0) {
            throw new ZeroDivisionException(left);
        }
        return (short)(left / right);
    }

    @Override
    public Short multiply(Short left, Short right) {
        return (short)(left * right);
    }

    @Override
    public Short negate(Short val) {
        return (short)(-val);
    }

    @Override
    public Short min(Short left, Short right) {
        return left < right ? left : right;
    }

    @Override
    public Short max(Short left, Short right) {
        return left > right ? left : right;
    }

    @Override
    public Short count(Short x) {
        return (short)(x < 0 ? Integer.bitCount(x) - 16 : Integer.bitCount(x));
    }
}
