package expression;

import expression.operation.OperableBigIntTable;
import expression.operation.OperableDoubleTable;
import expression.operation.OperableIntTable;
import expression.operation.OperableTable;

import java.math.BigInteger;
import java.util.Map;

public interface CommonExpression extends BaseExpression, TripleExpression {
    @Override
    default <T extends OperableTable<T, EvalT>, EvalT extends Number> EvalT evaluate(T table, EvalT x, EvalT y, EvalT z) {
        return evaluate(table, Map.of("x", x, "y", y, "z", z));
    }

    default int evaluate(int x, int y, int z) {
        return evaluate(OperableIntTable.getInstance(), Map.of("x", x, "y", y, "z", z));
    }

    default double evaluate(double x, double y, double z) {
        return evaluate(OperableDoubleTable.getInstance(), Map.of("x", x, "y", y, "z", z));
    }

    default BigInteger evaluate(BigInteger x, BigInteger y, BigInteger z) {
        return evaluate(OperableBigIntTable.getInstance(), Map.of("x", x, "y", y, "z", z));
    }
}
