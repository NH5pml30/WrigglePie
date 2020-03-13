package expression.operation;

import java.util.function.Consumer;

public enum BinaryOperationTableEntry implements OperationTableBase {
    ADD(2, "+", Add::new, Add::setEntry),
    SUBTRACT(2, "-", Subtract::new, Subtract::setEntry),
    MULTIPLY(1, "*", Multiply::new, Multiply::setEntry),
    DIVIDE(1, "/", Divide::new, Divide::setEntry),
    MIN(3, "min", Min::new, Min::setEntry),
    MAX(3, "max", Max::new, Max::setEntry);

    final int priority;
    final String symbol;
    final BinaryOperation.Factory factory;

    BinaryOperationTableEntry(int priority, String symbol, BinaryOperation.Factory factory, Consumer<BinaryOperationTableEntry> setter) {
        setter.accept(this);
        this.priority = priority;
        this.symbol = symbol;
        this.factory = factory;
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

    public BinaryOperation.Factory getFactory() {
        return factory;
    }
}
