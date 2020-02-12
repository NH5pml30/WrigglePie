package expression.operation;

public enum BinaryOperationTableEntry implements OperationTableBase {
    ADD(2, "+", true),
    SUBTRACT(2, "-", false),
    MULTIPLY(1, "*", true),
    DIVIDE(1, "/", false),
    POW(0, "**", false),
    LOG(0, "//", false);

    final int priority;
    final String symbol;
    final Boolean isAssoc;

    BinaryOperationTableEntry( int priority, String symbol, Boolean isAssoc ) {
        this.priority = priority;
        this.symbol = symbol;
        this.isAssoc = isAssoc;
    }

    @Override
    public String toString() {
        return symbol;
    }

    public boolean getAssoc() {
        return isAssoc;
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
