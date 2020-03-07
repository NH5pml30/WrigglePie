package expression;

import expression.operation.exception.EvaluationException;
import expression.operation.OperableIntTable;
import expression.parser.ExpressionParser;
import expression.parser.exception.ParserException;

public class DefaultMain {
    public static void main(String[] args) {
        String expression = "1000000*x*x*x*x*x/(x-1)";
        System.out.println("x\tf");
        CommonExpression expr;
        try {
            expr = new ExpressionParser().parse(expression);
        } catch (ParserException e) {
            System.out.println("Cannot run default test: " + e.getMessage());
            return;
        }

        for (int x = 0; x <= 10; x++) {
            System.out.print(x);
            System.out.print("\t");
            try {
                System.out.println(expr.evaluate(OperableIntTable.getInstance(), x, 0, 0));
            } catch (EvaluationException e) {
                System.out.println(e.getMessage());
            }
        }
    }
}
