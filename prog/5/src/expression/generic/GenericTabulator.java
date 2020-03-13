package expression.generic;

import expression.CommonExpression;
import expression.operation.OperableUncheckedIntTable;
import expression.operation.exception.EvaluationException;
import expression.parser.ExpressionParser;

import java.math.BigInteger;

public class GenericTabulator implements Tabulator {
    interface Evaluator {
        Object count(int x, int y, int z);
    }

    @Override
    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) throws Exception {
        CommonExpression expr = new ExpressionParser().parse(expression);
        Evaluator count;
        switch (mode) {
            case "i":
                count = expr::evaluate;
                break;
            case "d":
                count = (x, y, z) ->
                            expr.evaluate((double)x, (double)y, (double)z);
                break;
            case "bi":
                count = (x, y, z) ->
                            expr.evaluate(BigInteger.valueOf(x), BigInteger.valueOf(y), BigInteger.valueOf(z));
                break;
            case "u":
                count = (x, y, z) ->
                        expr.evaluate(OperableUncheckedIntTable.getInstance(), x, y, z);
                break;
            case "s":
                count = (x, y, z) ->
                        expr.evaluate((short)x, (short)y, (short)z);
                break;
            case "l":
                count = (x, y, z) ->
                        expr.evaluate((long)x, (long)y, (long)z);
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
