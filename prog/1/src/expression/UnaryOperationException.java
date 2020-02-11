package expression;

public class UnaryOperationException extends EvaluationException {
    private Number val;
    private String op;

    public UnaryOperationException( Number val, String op, String message ) {
        super(op + val, message);
        this.val = val;
        this.op = op;
    }

    public Number getVal() {
        return val;
    }

    public String getOp() {
        return op;
    }
}
