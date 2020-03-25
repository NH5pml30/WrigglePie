package expression.generic;

import expression.BaseTest;
import expression.CommonExpression;
import expression.operation.*;
import expression.operation.exception.EvaluationException;
import expression.parser.ExpressionParser;
import expression.parser.exception.ParserException;

import java.math.BigInteger;

public class GenericTabulator implements Tabulator {
    interface Evaluator {
        Object count(int x, int y, int z);
    }

    @Override
    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) throws ParserException, IllegalArgumentException {
        Evaluator count;
        switch (mode) {
            case "i":
                CommonExpression<OperableIntTable, Integer> iExpr =
                    new ExpressionParser<>(OperableIntTable.getInstance()).parse(expression);
                count = iExpr::evaluate;
                break;
            case "d":
                CommonExpression<OperableDoubleTable, Double> dExpr =
                    new ExpressionParser<>(OperableDoubleTable.getInstance()).parse(expression);
                count = (x, y, z) ->
                    dExpr.evaluate((double) x, (double) y, (double) z);
                break;
            case "bi":
                CommonExpression<OperableBigIntTable, BigInteger> biExpr =
                    new ExpressionParser<>(OperableBigIntTable.getInstance()).parse(expression);
                count = (x, y, z) ->
                    biExpr.evaluate(
                        BigInteger.valueOf(x), BigInteger.valueOf(y), BigInteger.valueOf(z)
                    );
                break;
            case "u":
                CommonExpression<OperableUncheckedIntTable, Integer> uExpr =
                    new ExpressionParser<>(OperableUncheckedIntTable.getInstance()).parse(expression);
                count = uExpr::evaluate;
                break;
            case "s":
                CommonExpression<OperableShortTable, Short> sExpr =
                    new ExpressionParser<>(OperableShortTable.getInstance()).parse(expression);
                count = (x, y, z) ->
                    sExpr.evaluate((short)x, (short)y, (short)z);
                break;
            case "l":
                CommonExpression<OperableLongTable, Long> lExpr =
                    new ExpressionParser<>(OperableLongTable.getInstance()).parse(expression);
                count = (x, y, z) ->
                    lExpr.evaluate((long)x, (long)y, (long)z);
                break;
            default:
                throw new IllegalArgumentException("Unknown mode: " + mode);
        }
        Object[][][] res = new Object[x2 - x1 + 1][y2 - y1 + 1][z2 - z1 + 1];
        for (int i = 0; i <= x2 - x1; i++) {
            for (int j = 0; j <= y2 - y1; j++) {
                for (int k = 0; k <= z2 - z1; k++) {
                    try {
                        res[i][j][k] = count.count(x1 + i, y1 + j, z1 + k);
                    } catch (EvaluationException e) {
                        res[i][j][k] = null;
                    }
                }
            }
        }
        return res;
    }
}
