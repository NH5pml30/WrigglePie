package expression.operation;

public enum BinaryOperationTableEntry implements OperationTableBase {
    ADD(2, "+"),
    SUBTRACT(2, "-"),
    MULTIPLY(1, "*"),
    DIVIDE(1, "/"),
    MIN(3, "min"),
    MAX(3, "max");

    final int priority;
    final String symbol;

    BinaryOperationTableEntry(int priority, String symbol) {
        this.priority = priority;
        this.symbol = symbol;
    }

    @Override
    public String toString() {
        return symbol;
    }

    @Override
    public int getPriority() {
        return priority;
    }

    @Override
    public String getSymbol() {
        return symbol;
    }
}
