package expression.parser.exception;

public class ParserException extends Exception {
    public ParserException(int at, String context, String message) {
        super("Error parsing expression at " + at + " (" + context + "): " + message);
    }
}
