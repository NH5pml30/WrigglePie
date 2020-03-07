package expression;

import expression.operation.exception.EvaluationException;
import expression.operation.OperableBigIntTable;
import expression.operation.OperableDoubleTable;
import expression.operation.OperableIntTable;
import expression.parser.ExpressionParser;
import expression.parser.exception.ParserException;

import java.math.BigInteger;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeSet;

public class ParserMain {
    private static int getInt(final Scanner scan, final String message) {
        System.out.print(message);
        while (!scan.hasNextInt()) {
            System.out.println("Invalid format");
            scan.nextLine();
            System.out.print(message);
        }
        int res = scan.nextInt();
        scan.nextLine();
        return res;
    }

    private static double getDouble(final Scanner scan, final String message) {
        System.out.print(message);
        while (!scan.hasNextDouble()) {
            System.out.println("Invalid format");
            scan.nextLine();
            System.out.print(message);
        }
        double res = scan.nextDouble();
        scan.nextLine();
        return res;
    }

    public static void main(String[] args) {
        if (args.length < 1 || !args[0].equals("-i") && !args[0].equals("-d") && !args[0].equals("-bi")) {
            System.out.println("Evaluation modes:\n-i -- int32\n-d -- double\n-bi -- big integer");
            return;
        }

        Scanner scanner = new Scanner(System.in);
        System.out.println("Input expression with variables x, y, z: ");
        String expression = scanner.nextLine();
        try {
            Set<String> names = Set.of("x", "y", "z"), outNames = new TreeSet<>();
            CommonExpression expr = new ExpressionParser().parse(expression, names, outNames);
            System.out.println("Got: " + expr.toString());

            switch (args[0]) {
                case "-i":
                    int
                        xi = outNames.contains("x") ? getInt(scanner, "Input x: ") : 0,
                        yi = outNames.contains("y") ? getInt(scanner, "Input y: ") : 0,
                        zi = outNames.contains("z") ? getInt(scanner, "Input z: ") : 0;
                    System.out.println(expr.evaluate(OperableIntTable.getInstance(), xi, yi, zi));
                    break;
                case "-d":
                    double
                        xd = outNames.contains("x") ? getDouble(scanner, "Input x: ") : 0,
                        yd = outNames.contains("y") ? getDouble(scanner, "Input y: ") : 0,
                        zd = outNames.contains("z") ? getDouble(scanner, "Input z: ") : 0;
                    System.out.println(expr.evaluate(OperableDoubleTable.getInstance(), xd, yd, zd));
                    break;
                case "-bi":
                    BigInteger
                        xbi = outNames.contains("x") ?
                                  BigInteger.valueOf(getInt(scanner, "Input x: ")) : BigInteger.ZERO,
                        ybi = outNames.contains("y") ?
                                  BigInteger.valueOf(getInt(scanner, "Input y: ")) : BigInteger.ZERO,
                        zbi = outNames.contains("z") ?
                                  BigInteger.valueOf(getInt(scanner, "Input z: ")) : BigInteger.ZERO;
                    System.out.println(expr.evaluate(OperableBigIntTable.getInstance(), xbi, ybi, zbi));
                    break;
            }
        } catch (ParserException e) {
            System.out.println(e.getMessage());
        } catch (EvaluationException e) {
            System.out.println(e.getMessage() + ": " + e.getExpr());
        }

        scanner.close();
    }
}
