package expression;

import expression.operation.OperableTable;
import expression.operation.exception.ConstFormatException;

import java.util.Map;
import java.util.function.Function;

public class Const<T extends OperableTable<T, EvalT>, EvalT extends Number>
        extends CommonExpression<T, EvalT> {
    private final EvalT val;

    public Const(final T table, final String strVal) throws ConstFormatException {
        super(table);
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

    // :NOTE: why this is not in abstraction?
    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Const<?, ?> baseConst = (Const<?, ?>)o;
        return table == baseConst.table && val.equals(baseConst.val);
    }

    @Override
    public int hashCode() {
        return val.hashCode();
    }
}
