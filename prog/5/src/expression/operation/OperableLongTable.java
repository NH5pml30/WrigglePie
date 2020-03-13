package expression.operation;

import expression.operation.exception.ZeroDivisionException;

public final class OperableLongTable implements OperableTable<OperableLongTable, Long> {
    private static final OperableLongTable instance = new OperableLongTable();

    public static OperableLongTable getInstance() {
        return instance;
    }

    @Override
    public Long parseNumber(String str) {
        return Long.parseLong(str);
    }

    @Override
    public Long add(Long left, Long right) {
        return left + right;
    }

    @Override
    public Long subtract(Long left, Long right) {
        return left - right;
    }

    @Override
    public Long divide(Long left, Long right) {
        if (right == 0) {
            throw new ZeroDivisionException(left);
        }
        return left / right;
    }

    @Override
    public Long multiply(Long left, Long right) {
        return left * right;
    }

    @Override
    public Long negate(Long val) {
        return -val;
    }

    @Override
    public Long min(Long left, Long right) {
        return Long.min(left, right);
    }

    @Override
    public Long max(Long left, Long right) {
        return Long.max(left, right);
    }

    @Override
    public Long count(Long x) {
        return (long)Long.bitCount(x);
    }
}
