package expression.operation.exception;

import expression.parser.exception.ParserException;

public class ConstFormatException extends NumberFormatException {
    private final String strVal;

    public ConstFormatException(String type, String strVal) {
        super("Cannot parse literal as " + type);
        this.strVal = strVal;
    }

    public String getStrVal() {
        return strVal;
    }
}
