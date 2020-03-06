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
    public Double pow(Double base, Double exp) {
        return Math.pow(base, exp);
    }

    @Override
    public Double log(Double x, Double base) {
        return Math.log(x) / Math.log(base);
    }

    @Override
    public Double log2(Double x) {
        return log(x, 2.0);
    }

    @Override
    public Double pow2(Double x) {
        return Math.pow(2, x);
    }
}
