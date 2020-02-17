package expression.exception;

public class ReadNumberException extends ExpressionException {
    public ReadNumberException( String message ) {
        super("Error while reading number: " + message);
    }
}
