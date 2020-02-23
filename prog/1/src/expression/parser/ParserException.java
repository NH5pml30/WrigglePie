package expression.parser;

public class ParserException extends Exception {
    public ParserException( String message ) {
        super("Error parsing expression at " + message);
    }
}
