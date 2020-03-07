package expression.parser;

import expression.CommonExpression;
import expression.parser.exception.ParserException;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface Parser {
    CommonExpression parse(String expression) throws ParserException;
}
