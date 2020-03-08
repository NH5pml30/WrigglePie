package expression.operation;

public interface OperableTable<
                                  T extends OperableTable<T, EvalT>,
                                  EvalT extends Number
                                  > {
    EvalT parseNumber(String str);

    EvalT add(EvalT left, EvalT right);

    EvalT subtract(EvalT left, EvalT right);

    EvalT divide(EvalT left, EvalT right);

    EvalT multiply(EvalT left, EvalT right);

    EvalT negate(EvalT x);

    EvalT log(EvalT base, EvalT x);

    EvalT log2(EvalT x);

    EvalT pow(EvalT base, EvalT exp);

    EvalT pow2(EvalT x);
}
