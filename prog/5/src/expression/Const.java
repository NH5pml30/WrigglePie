package expression;

import expression.operation.OperableTable;
import expression.operation.exception.ConstFormatException;

import java.util.Map;
import java.util.function.Function;

public class Const<T extends OperableTable<T, EvalT>, EvalT extends Number>
        implements CommonExpression<T, EvalT> {
    private final EvalT val;

    public Const(final T table, final String strVal) throws ConstFormatException {
        this.val = table.parseNumber(strVal);
    }

    @Override
    public EvalT evaluate(final Map<String, EvalT> x) {
        return val;
    }

    @Override
    public String toString() {
        return val.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Const<T, EvalT> baseConst = (Const<T, EvalT>)o;
        return val.equals(baseConst.val);
    }

    @Override
    public int hashCode() {
        return val.hashCode();
    }
}
