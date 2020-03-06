package expression;

import expression.operation.OperableTable;

import java.util.Map;

public class Const implements CommonExpression {
    private final String strVal;

    public Const(final String strVal) {
        this.strVal = strVal;
    }

    @Override
    public <T extends OperableTable<T, EvalT>, EvalT extends Number>
    EvalT evaluate(T table, Map<String, EvalT> x) {
        return table.parseNumber(strVal);
    }

    @Override
    public String toString() {
        return strVal;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Const baseConst = (Const)o;
        return strVal.equals(baseConst.strVal);
    }

    @Override
    public int hashCode() {
        return strVal.hashCode();
    }
}
