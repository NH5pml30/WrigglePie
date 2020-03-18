package expression;

import expression.operation.exception.EvaluationException;
import expression.parser.ExpressionParser;
import expression.parser.exception.ParserException;
import expression.parser.exception.UnrecognizedTokenException;

import java.math.BigInteger;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;

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

    private static <T> void getNumbers(String[] args, int offset, Function<String, T> parse,
                                       T[] res, Set<String> names) {
        for (String name : names) {
            int ind = name.charAt(0) - 'x';
            res[ind] = parse.apply(args[ind + offset]);
        }
    }

    public static void main(String[] args) {
        if (args.length < 2 || !args[0].equals("-i") && !args[0].equals("-d") && !args[0].equals("-bi")) {
            System.out.println("Usage: <executable> -mode expression [-x [-y [-z]]]");
            System.out.println("Evaluation modes:\n-i -- int32\n-d -- double\n-bi -- big integer");
            return;
        }

        String expression = args[1];
        Set<String> names, outNames = new TreeSet<>();
        try {
            switch (args.length) {
                case 5:
                    names = Set.of("x", "y", "z");
                    break;
                case 4:
                    names = Set.of("x", "y");
                    break;
                case 3:
                    names = Set.of("x");
                    break;
                default:
                    names = Set.of();
                    break;
            }
            CommonExpression expr = new ExpressionParser().parse(expression, names, outNames);
            System.out.println("Got: " + expr.toString());

            switch (args[0]) {
                case "-i":
                    Integer[] iVals = new Integer[] {0, 0, 0};
                    getNumbers(args, 2, Integer::parseInt, iVals, outNames);
                    System.out.println(expr.evaluate(iVals[0], iVals[1], iVals[2]));
                    break;
                case "-d":
                    Double[] dVals = new Double[] {0.0, 0.0, 0.0};
                    getNumbers(args, 2, Double::parseDouble, dVals, outNames);
                    System.out.println(expr.evaluate(dVals[0], dVals[1], dVals[2]));
                    break;
                case "-bi":
                    BigInteger[] biVals = new BigInteger[] {BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO};
                    getNumbers(args, 2, BigInteger::new, biVals, outNames);
                    System.out.println(expr.evaluate(biVals[0], biVals[1], biVals[2]));
                    break;
            }
        } catch (UnrecognizedTokenException e) {
            switch (e.getToken()) {
                case "x":
                case "y":
                case "z":
                    System.out.println("Not enough variable values given! (used at least '" + e.getToken() + "')");
                    break;
                default:
                    System.out.println(e.getMessage());
            }
        } catch (ParserException e) {
            System.out.println(e.getMessage());
        } catch (EvaluationException e) {
            System.out.println(e.getMessage() + ": " + e.getExpr());
        }
    }
}
