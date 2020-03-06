package expression;

import expression.operation.OperableTable;

import java.util.Map;

public class Variable implements CommonExpression {
    private final String name;

    public Variable(String name) {
        this.name = name;
    }

    @Override
    public <T extends OperableTable<T, EvalT>, EvalT extends Number>
    EvalT evaluate(T table, Map<String, EvalT> x) {
        return x.get(name);
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public boolean equals(Object o) {
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
