package expression;

public class EvaluationException extends ExpressionException {
    private String expr;

    public EvaluationException( String expr, String message ) {
        super(message);
        this.expr = expr;
    }

    void addExpr(String expr) {
        this.expr += expr;
    }

    String getExpr() {
        return expr;
    }
}
