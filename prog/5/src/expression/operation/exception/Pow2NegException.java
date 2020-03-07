package expression.operation.exception;

public class Pow2NegException extends UnaryOperationException {
    public Pow2NegException(Number val) {
        super(val, "pow2 of negative argument (makes no sense in integers)");
    }
}
