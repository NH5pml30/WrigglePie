package expression;

import expression.exception.EvaluationException;
import expression.exception.ExpressionException;
import expression.parser.*;

import java.util.Scanner;
import java.util.Set;
import java.util.TreeSet;

public class DefaultMain {
    public static void main( String[] args ) {
        String expression = "1000000*x*x*x*x*x/(x-1)";
        for (int x = 0; x <= 10; x++) {
            System.out.print(x);
            System.out.print("\t");
            try {
                CommonExpression expr = new ExpressionParser().parse(expression);
                System.out.println(expr.evaluate(x, 0, 0));
            } catch (ExpressionException e) {
                System.out.println(e.getMessage());
            }
        }
    }
}
