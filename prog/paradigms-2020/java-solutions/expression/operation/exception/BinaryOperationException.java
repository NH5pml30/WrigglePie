package expression.operation.exception;

public class BinaryOperationException extends EvaluationException {
    private Number left, right;
    private String op;

    public BinaryOperationException(Number left, Number right, String message) {
        super(message);
        this.left = left;
        this.right = right;
    }

    @Override
    public void setOp(String op) {
        this.op = op;
        super.addExpr(left + " " + op + " " + right);
    }

    public Number getLeft() {
        return left;
    }

    public Number getRight() {
        return right;
    }

    public String getOp() {
        return op;
    }
}
