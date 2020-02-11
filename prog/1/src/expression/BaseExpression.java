package expression;

import java.util.Map;

public interface BaseExpression extends ToMiniString {
    int evaluate( Map<String, Integer> x );
}
