package expression.operation.exception;

public class ConstFormatException extends EvaluationException {
    public ConstFormatException(String type, String strVal) {
        super("Cannot parse literal as " + type);
        addExpr(strVal);
        setFilled();
    }
}
