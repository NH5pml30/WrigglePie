package expression;

public class BinaryOperationException extends EvaluationException {
    private Number left, right;
    private String op;

    public BinaryOperationException( Number left, Number right, String op, String message ) {
        super(left + " " + op + ' ' + right, message);
        this.left = left;
        this.right = right;
        this.op = op;
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
