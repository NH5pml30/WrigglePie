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
    private static abstract class ExprFactory implements Comparable<ExprFactory> {
        final OperationTableBase entry;

        ExprFactory( OperationTableBase entry ) {
            this.entry = entry;
        }

        @Override
        public int compareTo( ExprFactory other ) {
            return entry.comparePrior(other.entry);
        }
    }

    private static class BiExprFactory extends ExprFactory {
        private final BiFunction<CommonExpression, CommonExpression, CommonExpression> factory;

        BiExprFactory( BiFunction<CommonExpression, CommonExpression, CommonExpression> factory, BinaryOperationTableEntry entry ) {
            super(entry);
            this.factory = factory;
        }

        CommonExpression create( CommonExpression left, CommonExpression right ) {
            return factory.apply(left, right);
        }
    }

    private static class UnExprFactory extends ExprFactory {
        private final Function<CommonExpression, CommonExpression> factory;

        UnExprFactory( Function<CommonExpression, CommonExpression> factory, UnaryOperationTableEntry entry ) {
            super(entry);
            this.factory = factory;
        }

        CommonExpression create( CommonExpression expr ) {
            return factory.apply(expr);
        }
    }

    private final static Map<BinaryOperationTableEntry, BiExprFactory>
        biFactories = Map.of(
            CheckedAdd.entry, new BiExprFactory(CheckedAdd::new, CheckedAdd.entry),
            CheckedSubtract.entry, new BiExprFactory(CheckedSubtract::new, CheckedSubtract.entry),
            CheckedDivide.entry, new BiExprFactory(CheckedDivide::new, CheckedDivide.entry),
            CheckedMultiply.entry, new BiExprFactory(CheckedMultiply::new, CheckedMultiply.entry),
            CheckedPow.entry, new BiExprFactory(CheckedPow::new, CheckedPow.entry),
            CheckedLog.entry, new BiExprFactory(CheckedLog::new, CheckedLog.entry)
        );
    private final static Map<UnaryOperationTableEntry, UnExprFactory>
        unFactories = Map.of(
            CheckedNegate.entry, new UnExprFactory(CheckedNegate::new, CheckedNegate.entry),
            CheckedLog2.entry, new UnExprFactory(CheckedLog2::new, CheckedLog2.entry),
            CheckedPow2.entry, new UnExprFactory(CheckedPow2::new, CheckedPow2.entry)
        );

    public ExpressionParser() {
    }

    public CommonExpression parse( final String source, Set<String> inVarNames, Set<String> outVarNames ) {
        return parse(new StringSource(source), inVarNames, outVarNames);
    }

    public CommonExpression parse( final String source, Set<String> outVarNames ) {
        return parse(new StringSource(source), Set.of("x", "y", "z"), outVarNames);
    }

    public CommonExpression parse( final String source ) {
        return parse(new StringSource(source), Set.of("x", "y", "z"), null);
    }

    private CommonExpression parse( ExpressionSource source, Set<String> inVarNames, Set<String> outVarNames ) {
        return new InnerExprParser(source, inVarNames).parse(outVarNames);
    }

    private static class InnerExprParser extends BaseParser {
        private Set<String> constraintVarNames;

        InnerExprParser( final ExpressionSource source, Set<String> constraintVarNames ) {
            super(source);
            this.constraintVarNames = constraintVarNames;
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
                if (constraintVarNames != null && !constraintVarNames.contains(tokenData.name)) {
                    throw error("variable name " + tokenData.name + " not supported!");
                }
                if (outVarNames != null) {
                    outVarNames.add(tokenData.name);
                }
                nextToken();
                left = new Variable(lastData.name);
            } else if (test(TokenType.UNARY_OP)) {
                left = unFactories.get(lastData.unOp).create(parseSubexpression(lastData.unOp.getPriority(), outVarNames));
            } else {
                expect(TokenType.NUMBER);
                left = new Const(lastData.val);
            }

            while (true) {
                if (testNoShift(TokenType.RIGHT_PAR) || testNoShift(TokenType.NONE)) {
                    return left;
                }

                expectNoShift(TokenType.BINARY_OP);
                if (tokenData.biOp.getPriority() >= lastPriority) {
                    return left;
                } else {
                    nextToken();
                    int prior = lastData.biOp.getPriority();
                    left = biFactories.get(lastData.biOp).create(left, parseSubexpression(prior, outVarNames));
                }
            }
        }
    }
}
