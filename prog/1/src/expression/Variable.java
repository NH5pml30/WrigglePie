package expression;

import java.util.Map;
import java.util.function.Function;

public class Variable implements CommonExpression {
    private final String name;

    public Variable( String name ) {
        this.name = name;
    }

    @Override
    public int evaluate( Map<String, Integer> x ) {
        return x.get(name);
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public boolean equals( Object o ) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Variable variable = (Variable)o;
        return name.equals(variable.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }
}
