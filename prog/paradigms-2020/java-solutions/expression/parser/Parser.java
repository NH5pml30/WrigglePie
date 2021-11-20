package expression.parser;

import expression.CommonExpression;
import expression.operation.OperableTable;
import expression.parser.exception.ParserException;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface Parser<T extends OperableTable<T, EvalT>, EvalT extends Number> {
    CommonExpression<T, EvalT> parse(String expression) throws ParserException;
}
