package expression.parser.exception;

public class UnexpectedTokenException extends ParserException {
    public UnexpectedTokenException(int at, String context, String message) {
        super(at, context, "unexpected token: " + message);
    }
}
