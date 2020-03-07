package expression.parser;

import expression.CommonExpression;
import expression.Const;
import expression.Variable;
import expression.operation.*;
import expression.parser.ExpressionSource.TokenType;
import expression.parser.exception.ParserException;

import java.util.Map;
import java.util.Set;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 * @author Nikolai Kholiavin
 */
public class ExpressionParser implements Parser {
    private interface BiExprFactory {
        CommonExpression create(
            CommonExpression left,
            CommonExpression right
        );
    }

    private interface UnExprFactory {
        CommonExpression create(CommonExpression left);
    }

    private static final Map<BinaryOperationTableEntry, BiExprFactory>
        biFactories = Map.of(
        Add.entry, Add::new,
        Subtract.entry, Subtract::new,
        Divide.entry, Divide::new,
        Multiply.entry, Multiply::new,
        Pow.entry, Pow::new,
        Log.entry, Log::new
    );
    private static final Map<UnaryOperationTableEntry, UnExprFactory>
        unFactories = Map.of(
        Negate.entry, Negate::new,
        Log2.entry, Log2::new,
        Pow2.entry, Pow2::new
    );

    public ExpressionParser() {
    }

    public CommonExpression parse(final String source, Set<String> inVarNames, Set<String> outVarNames)
        throws ParserException {
        return parse(new StringSource(source, inVarNames), outVarNames);
    }

    public CommonExpression parse(final String source, Set<String> outVarNames)
        throws ParserException {
        return parse(new StringSource(source, Set.of("x", "y", "z")), outVarNames);
    }

    public CommonExpression parse(final String source)
        throws ParserException {
        return parse(new StringSource(source, Set.of("x", "y", "z")), null);
    }

    private CommonExpression parse(ExpressionSource source, Set<String> outVarNames)
        throws ParserException {
        return new InnerExprParser(source).parse(outVarNames);
    }

    private static class InnerExprParser extends BaseParser {
        InnerExprParser(final ExpressionSource source) throws ParserException {
            super(source);
            nextToken();
        }

        private CommonExpression parse(Set<String> outVarNames) throws ParserException {
            final CommonExpression res = parseSubexpression(Integer.MAX_VALUE, outVarNames);
            expect(TokenType.NONE);
            return res;
        }

        private CommonExpression parseSubexpression(int lastPriority, Set<String> outVarNames) throws ParserException {
            CommonExpression left;
            if (test(TokenType.LEFT_PAR)) {
                left = parseSubexpression(Integer.MAX_VALUE, outVarNames);
                expect(TokenType.RIGHT_PAR);
            } else if (testNoShift(TokenType.NAME)) {
                if (outVarNames != null) {
                    outVarNames.add(tokenData.str);
                }
                nextToken();
                left = new Variable(lastData.str);
            } else if (test(TokenType.UNARY_OP)) {
                UnaryOperationTableEntry op = lastData.uOp;
                left = unFactories.get(op).create(parseSubexpression(op.getPriority(), outVarNames));
            } else {
                expect(TokenType.NUMBER);
                left = new Const(lastData.str);
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
                    left = biFactories.get(op).create(left, parseSubexpression(prior, outVarNames));
                }
            }
        }
    }
}
