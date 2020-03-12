package expression.operation;

import expression.operation.exception.*;

public final class OperableUncheckedIntTable implements OperableTable<OperableUncheckedIntTable, Integer> {
    private static final OperableUncheckedIntTable instance = new OperableUncheckedIntTable();

    public static OperableUncheckedIntTable getInstance() {
        return instance;
    }

    @Override
    public Integer parseNumber(String str) {
        return OperableIntTable.getInstance().parseNumber(str);
    }

    @Override
    public Integer add(Integer left, Integer right) {
        return left + right;
    }

    @Override
    public Integer subtract(Integer left, Integer right) {
        return left - right;
    }

    @Override
    public Integer divide(Integer left, Integer right) {
        return left / right;
    }

    @Override
    public Integer multiply(Integer left, Integer right) {
        return left * right;
    }

    @Override
    public Integer negate(Integer val) {
        return -val;
    }

    @Override
    public Integer min(Integer left, Integer right) {
        return OperableIntTable.getInstance().min(left, right);
    }

    @Override
    public Integer max(Integer left, Integer right) {
        return OperableIntTable.getInstance().max(left, right);
    }

    @Override
    public Integer count(Integer x) {
        return OperableIntTable.getInstance().count(x);
    }
}
