package expression.parser;

import expression.*;
import expression.operation.*;
import expression.parser.ExpressionSource.TokenType;

import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 * @author Nikolai Kholiavin
 */
public class ExpressionParser implements Parser {
    private interface BiExprFactory {
        CommonExpression create( CommonExpression left, CommonExpression right );
    }

    private interface UnExprFactory {
        CommonExpression create( CommonExpression left );
    }

    private final static Map<BinaryOperationTableEntry, BiExprFactory>
        biFactories = Map.of(
            CheckedAdd.entry, CheckedAdd::new,
            CheckedSubtract.entry, CheckedSubtract::new,
            CheckedDivide.entry, CheckedDivide::new,
            CheckedMultiply.entry, CheckedMultiply::new,
            CheckedPow.entry, CheckedPow::new,
            CheckedLog.entry, CheckedLog::new
        );
    private final static Map<UnaryOperationTableEntry, UnExprFactory>
        unFactories = Map.of(
            CheckedNegate.entry, CheckedNegate::new,
            CheckedLog2.entry, CheckedLog2::new,
            CheckedPow2.entry, CheckedPow2::new
        );

    public ExpressionParser() {
    }

    public CommonExpression parse( final String source, Set<String> inVarNames, Set<String> outVarNames ) {
        return parse(new StringSource(source, inVarNames), outVarNames);
    }

    public CommonExpression parse( final String source, Set<String> outVarNames ) {
        return parse(new StringSource(source, Set.of("x", "y", "z")), outVarNames);
    }

    public CommonExpression parse( final String source ) {
        return parse(new StringSource(source, Set.of("x", "y", "z")), null);
    }

    private CommonExpression parse( ExpressionSource source, Set<String> outVarNames ) {
        return new InnerExprParser(source).parse(outVarNames);
    }

    private static class InnerExprParser extends BaseParser {
        InnerExprParser( final ExpressionSource source ) {
            super(source);
            nextToken();
        }

        private CommonExpression parse( Set<String> outVarNames ) {
            final CommonExpression res = parseSubexpression(Integer.MAX_VALUE, outVarNames);
            expect(TokenType.NONE);
            return res;
        }

        private CommonExpression parseSubexpression( int lastPriority, Set<String> outVarNames ) {
            CommonExpression left;
            if (test(TokenType.LEFT_PAR)) {
                left = parseSubexpression(Integer.MAX_VALUE, outVarNames);
                expect(TokenType.RIGHT_PAR);
            } else if (testNoShift(TokenType.NAME)) {
                if (outVarNames != null) {
                    outVarNames.add(tokenData.get());
                }
                nextToken();
                left = new Variable(lastData.get());
            } else if (test(TokenType.UNARY_OP)) {
                UnaryOperationTableEntry op = lastData.get();
                left = unFactories.get(op).create(parseSubexpression(op.getPriority(), outVarNames));
            } else {
                expect(TokenType.NUMBER);
                left = new Const(lastData.get());
            }

            while (true) {
                if (testNoShift(TokenType.RIGHT_PAR) || testNoShift(TokenType.NONE)) {
                    return left;
                }

                expectNoShift(TokenType.BINARY_OP);
                BinaryOperationTableEntry op = tokenData.get();
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
