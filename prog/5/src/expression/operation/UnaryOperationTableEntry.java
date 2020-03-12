package expression.operation;

public enum UnaryOperationTableEntry implements OperationTableBase {
    UNARY_MINUS(0, "-"),
    COUNT(0, "count");

    final int priority;
    final String symbol;

    UnaryOperationTableEntry(int priority, String symbol) {
        this.priority = priority;
        this.symbol = symbol;
    }

    @Override
    public int getPriority() {
        return priority;
    }

    @Override
    public String getSymbol() {
        return symbol;
    }

    @Override
    public String toString() {
        return symbol;
    }
}
