package expression.parser;

import expression.exception.ExpressionException;

public class ParserException extends ExpressionException {
    public ParserException( String message ) {
        super("Error parsing expression at " + message);
    }
}
