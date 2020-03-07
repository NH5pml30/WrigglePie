package expression.operation.exception;

public abstract class EvaluationException extends RuntimeException {
    private String expr = "";
    private boolean isFilled = false;

    public EvaluationException(String message) {
        super(message);
    }

    public void setOp(String op) {}

    public void addExpr(String expr) {
        this.expr += expr;
    }

    public void setFilled() {
        isFilled = true;
    }

    public boolean getFilled() {
        return isFilled;
    }

    public String getExpr() {
        return expr;
    }
}
