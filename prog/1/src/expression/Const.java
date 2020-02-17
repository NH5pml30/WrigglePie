package expression;

import expression.exception.ReadNumberException;
import expression.operation.OperableInt;

import java.util.Map;
import java.util.function.Function;

public class Const implements CommonExpression {
    private final int value;

    public Const( final int value ) {
        this.value = value;
    }

    public Const( final String strVal ) {
        this.value = OperableInt.parse(strVal);
    }

    @Override
    public int evaluate( Map<String, Integer> x ) {
        return value;
    }

    @Override
    public String toString() {
        return Integer.toString(value);
    }

    @Override
    public boolean equals( Object o ) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Const baseConst = (Const)o;
        return value == baseConst.value;
    }

    @Override
    public int hashCode() {
        return Integer.hashCode(value);
    }
}
