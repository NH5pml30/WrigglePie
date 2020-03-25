package expression;

import expression.operation.OperableTable;

import java.util.Map;

public class Variable<T extends OperableTable<T, EvalT>, EvalT extends Number>
        implements CommonExpression<T, EvalT> {
    private final String name;

    public Variable(String name) {
        this.name = name;
    }

    @Override
    public EvalT evaluate(final Map<String, EvalT> x) {
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
        Variable<T, EvalT> variable = (Variable<T, EvalT>)o;
        return name.equals(variable.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }
}
