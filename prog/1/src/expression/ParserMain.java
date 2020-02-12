package expression;

import expression.exception.EvaluationException;
import expression.parser.*;

import java.util.Scanner;
import java.util.Set;
import java.util.TreeSet;

public class ParserMain {
    private static int getInt( final Scanner scan, final String message ) {
        System.out.print(message);
        while (!scan.hasNextInt()) {
            System.out.println("Invalid format");
            scan.nextLine();
            System.out.print(message);
        }
        return scan.nextInt();
    }

    public static void main( String[] args ) {
        Scanner scanner = new Scanner(System.in);
        System.out.println("Input expression with variables x, y, z: ");
        String expression = scanner.nextLine();
        try {
            Set<String> names = Set.of("x", "y", "z"), outNames = new TreeSet<>();
            CommonExpression expr = new ExpressionParser().parse(expression, names, outNames);
            System.out.println("Got: " + expr.toString());
            int
                x = outNames.contains("x") ? getInt(scanner, "Input x: ") : 0,
                y = outNames.contains("y") ? getInt(scanner, "Input y: ") : 0,
                z = outNames.contains("z") ? getInt(scanner, "Input z: ") : 0;
            System.out.println(expr.evaluate(x, y, z));
        } catch ( ParserException e ) {
            System.out.println(e.getMessage());
        } catch ( EvaluationException e ) {
            System.out.println(e.getMessage() + ": " + e.getExpr());
        }

        scanner.close();
    }
}
