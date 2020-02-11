package expression;

import java.util.Map;

public interface CommonExpression extends BaseExpression, Expression, TripleExpression {
    @Override
    default int evaluate( int x ) {
        return evaluate(Map.of("x", x));
    }

    @Override
    default int evaluate( int x, int y, int z ) {
        return evaluate(Map.of("x", x, "y", y, "z", z));
    }
}
