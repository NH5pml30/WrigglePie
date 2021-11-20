package expression.parser.exception;

public class UnrecognizedTokenException extends ParserException {
    private String token;

    public UnrecognizedTokenException(int at, String context, String token, String message) {
        super(at, context, "Unrecognized token '" + token + "': " + message);
        this.token = token;
    }

    public String getToken() {
        return token;
    }
}
