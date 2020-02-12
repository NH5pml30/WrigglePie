package expression.operation;

public enum UnaryOperationTableEntry implements OperationTableBase {
    UNARY_MINUS(0, "-"),
    LOG2(0, "log2"),
    POW2(0, "pow2");

    final int priority;
    final String symbol;

    UnaryOperationTableEntry( int priority, String symbol ) {
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
