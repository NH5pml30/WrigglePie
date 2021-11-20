package expression;

import expression.operation.OperableTable;

import java.util.Map;

public class Variable<T extends OperableTable<T, EvalT>, EvalT extends Number>
        extends CommonExpression<T, EvalT> {
    private final String name;

    public Variable(final T table, String name) {
        super(table);
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
        Variable<?, ?> variable = (Variable<?, ?>)o;
        return table == variable.table && name.equals(variable.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }
}
