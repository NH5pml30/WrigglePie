package expression;

import expression.operation.*;

import java.math.BigInteger;
import java.util.Map;

public abstract class CommonExpression<T extends OperableTable<T, EvalT>, EvalT extends Number>
        implements BaseExpression<T, EvalT>, TripleExpression<T, EvalT> {
    protected final T table;

    public CommonExpression(final T table) {
        this.table = table;
    }
    
    // :NOTE: Bad idea. Task require to compute all evaluations of expression in one type
    // Imaging that I added in some operator method:
    /*
     * public <T extends OperableTable<T, EvalT>, EvalT extends Number> EvalT evaluate(T table, EvalT x, EvalT y, EvalT z) {
     *     return op (left.evaluate (x, y, z), (EvalT) right.<OperableTable <Byte, ...>> evaluate ((Byte) x, (Byte) y, (Byte) z))
     * }
     *
     * In case of T != Byte computations will be in different types and nothing can stop me
     * (Parameterization should be on type level)
     */
    // new version:
    @Override
    public EvalT evaluate(EvalT x, EvalT y, EvalT z) {
        return evaluate(Map.of("x", x, "y", y, "z", z));
    }
}
