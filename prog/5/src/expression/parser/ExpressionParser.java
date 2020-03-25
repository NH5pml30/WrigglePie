package expression.parser;

import expression.CommonExpression;
import expression.Const;
import expression.Variable;
import expression.operation.*;
import expression.operation.exception.ConstFormatException;
import expression.parser.ExpressionSource.TokenType;
import expression.parser.exception.ParserException;
import expression.parser.exception.UnrecognizedTokenException;

import java.util.Map;
import java.util.Set;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 * @author Nikolai Kholiavin
 */
public class ExpressionParser<T extends OperableTable<T, EvalT>, EvalT extends Number>
        implements Parser<T, EvalT> {
    private final T table;

    public ExpressionParser(final T table) {
        this.table = table;
    }

    public CommonExpression<T, EvalT> parse(
            final String source,
            final Set<String> inVarNames, Set<String> outVarNames
    ) throws ParserException {
        return parse(new StringSource(source, inVarNames), outVarNames);
    }

    public CommonExpression<T, EvalT> parse(
            final String source,
            final Set<String> outVarNames
    ) throws ParserException {
        return parse(new StringSource(source, Set.of("x", "y", "z")), outVarNames);
    }

    @Override
    public CommonExpression<T, EvalT> parse(final String source)
        throws ParserException {
        return parse(new StringSource(source, Set.of("x", "y", "z")), null);
    }

    private CommonExpression<T, EvalT> parse(
            final ExpressionSource source,
            final Set<String> outVarNames
    ) throws ParserException {
        return new InnerExprParser<>(source, table).parse(outVarNames);
    }

    private static class InnerExprParser<T extends OperableTable<T, EvalT>, EvalT extends Number>
            extends BaseParser {
        private final T table;
        InnerExprParser(final ExpressionSource source, final T table) throws ParserException {
            super(source);
            this.table = table;
            nextToken();
        }

        private CommonExpression<T, EvalT> parse(Set<String> outVarNames) throws ParserException {
            final CommonExpression<T, EvalT> res = parseSubexpression(Integer.MAX_VALUE, outVarNames);
            expect(TokenType.NONE);
            return res;
        }

        private CommonExpression<T, EvalT> parseSubexpression(int lastPriority, Set<String> outVarNames) throws ParserException {
            CommonExpression<T, EvalT> left;
            if (test(TokenType.LEFT_PAR)) {
                left = parseSubexpression(Integer.MAX_VALUE, outVarNames);
                expect(TokenType.RIGHT_PAR);
            } else if (testNoShift(TokenType.NAME)) {
                if (outVarNames != null) {
                    outVarNames.add(tokenData.str);
                }
                nextToken();
                left = new Variable<>(lastData.str);
            } else if (test(TokenType.UNARY_OP)) {
                UnaryOperationTableEntry op = lastData.uOp;
                left = op.getFactory().create(table, parseSubexpression(op.getPriority(), outVarNames));
            } else {
                expect(TokenType.NUMBER);
                try {
                    left = new Const<>(table, lastData.str);
                } catch (NumberFormatException e) {
                    throw error(
                        (at, context, message) ->
                            new UnrecognizedTokenException(at, context, lastData.str, message),
                        e.getMessage()
                    );
                }
            }

            while (true) {
                if (testNoShift(TokenType.RIGHT_PAR) || testNoShift(TokenType.NONE)) {
                    return left;
                }

                expectNoShift(TokenType.BINARY_OP);
                BinaryOperationTableEntry op = tokenData.bOp;
                if (op.getPriority() >= lastPriority) {
                    return left;
                } else {
                    nextToken();
                    int prior = op.getPriority();
                    left = op.getFactory().create(table, left, parseSubexpression(prior, outVarNames));
                }
            }
        }
    }
}
