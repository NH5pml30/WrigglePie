package expression.operation;

public final class OperableDoubleTable implements OperableTable<OperableDoubleTable, Double> {
    private static final OperableDoubleTable instance = new OperableDoubleTable();

    public static OperableDoubleTable getInstance() {
        return instance;
    }

    @Override
    public Double parseNumber(String str) {
        return Double.parseDouble(str);
    }

    @Override
    public Double add(Double left, Double right) {
        return left + right;
    }

    @Override
    public Double subtract(Double left, Double right) {
        return left - right;
    }

    @Override
    public Double divide(Double left, Double right) {
        return left / right;
    }

    @Override
    public Double multiply(Double left, Double right) {
        return left * right;
    }

    @Override
    public Double negate(Double val) {
        return -val;
    }

    @Override
    public Double min(Double left, Double right) {
        return Double.min(left, right);
    }

    @Override
    public Double max(Double left, Double right) {
        return Double.max(left, right);
    }

    @Override
    public Double count(Double x) {
        return (double)Long.bitCount(Double.doubleToLongBits(x));
    }
}
