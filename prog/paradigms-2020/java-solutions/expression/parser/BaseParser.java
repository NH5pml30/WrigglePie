package expression.parser;

import expression.parser.ExpressionSource.TokenData;
import expression.parser.ExpressionSource.TokenType;
import expression.parser.exception.ParserException;
import expression.parser.exception.ParserExceptionCreator;
import expression.parser.exception.UnexpectedTokenException;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 * @author Nikolai Kholiavin
 */
public class BaseParser {
    private final ExpressionSource source;
    protected TokenType token;
    protected TokenData tokenData, lastData;

    protected BaseParser(final ExpressionSource source) {
        this.source = source;
    }

    protected void nextToken() throws ParserException {
        lastData = tokenData;
        token = source.next();
        tokenData = source.tokenData;
    }

    protected boolean testNoShift(TokenType expected) {
        return token == expected;
    }

    protected boolean test(TokenType expected) throws ParserException {
        if (testNoShift(expected)) {
            nextToken();
            return true;
        }
        return false;
    }

    protected void expectNoShift(TokenType c) throws ParserException {
        if (token != c) {
            throw error(UnexpectedTokenException::new, "Expected '" + c.represent + "', found '" + token.represent + "'");
        }
    }

    protected void expect(TokenType c) throws ParserException {
        expectNoShift(c);
        nextToken();
    }

    protected ParserException error(ParserExceptionCreator creator, final String message) {
        return source.error(creator, message);
    }
}
