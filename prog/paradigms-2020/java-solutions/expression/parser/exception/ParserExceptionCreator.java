package expression.parser.exception;

public interface ParserExceptionCreator {
    ParserException create(int at, String context, String message);
}
