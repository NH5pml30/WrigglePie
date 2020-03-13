package expression.operation;

import java.util.function.Consumer;

public enum UnaryOperationTableEntry implements OperationTableBase {
    UNARY_MINUS(0, "-", Negate::new, Negate::setEntry),
    LOG2(0, "log2", Log2::new, Log2::setEntry),
    POW2(0, "pow2", Pow2::new, Pow2::setEntry);

    final int priority;
    final String symbol;
    final UnaryOperation.Factory factory;

    UnaryOperationTableEntry(int priority, String symbol, UnaryOperation.Factory factory, Consumer<UnaryOperationTableEntry> setter) {
        setter.accept(this);
        this.priority = priority;
        this.symbol = symbol;
        this.factory = factory;
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

    public UnaryOperation.Factory getFactory() {
        return factory;
    }
}
