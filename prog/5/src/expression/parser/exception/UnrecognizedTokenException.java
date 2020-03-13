package expression.parser.exception;

public class UnrecognizedTokenException extends ParserException {
    public UnrecognizedTokenException(int at, String context, String message) {
        super(at, context, "Unrecognized token: " + message);
    }
}
