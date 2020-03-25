package expression.operation;

import expression.operation.exception.ConstFormatException;
import expression.parser.exception.ParserException;

public interface OperableTable<
                                  T extends OperableTable<T, EvalT>,
                                  EvalT extends Number
                                  > {
    EvalT parseNumber(String str) throws ConstFormatException;

    EvalT add(EvalT left, EvalT right);

    EvalT subtract(EvalT left, EvalT right);

    EvalT divide(EvalT left, EvalT right);

    EvalT multiply(EvalT left, EvalT right);

    EvalT negate(EvalT x);

    EvalT min(EvalT left, EvalT right);

    EvalT max(EvalT left, EvalT right);

    EvalT count(EvalT x);
}
