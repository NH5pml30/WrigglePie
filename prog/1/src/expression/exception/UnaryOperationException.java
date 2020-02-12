package expression.exception;

public class UnaryOperationException extends EvaluationException {
    private Number val;
    private String op;

    public UnaryOperationException( Number val, String message ) {
        super(message);
        this.val = val;
    }

    @Override
    public void setOp( String op ) {
        this.op = op;
        super.addExpr(op + '(' + val + ')');
    }

    public Number getVal() {
        return val;
    }

    public String getOp() {
        return op;
    }
}
