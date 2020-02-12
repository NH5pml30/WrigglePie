package expression.operation;

public interface OperationTableBase {
    int getPriority();

    String getSymbol();

    default int comparePrior( OperationTableBase other ) {
        return getPriority() - other.getPriority();
    }
}
