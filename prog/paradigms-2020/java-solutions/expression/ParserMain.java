package expression;

import expression.operation.OperableBigIntTable;
import expression.operation.OperableDoubleTable;
import expression.operation.OperableIntTable;
import expression.operation.OperableTable;
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

    private static <T extends OperableTable<T, EvalT>, EvalT extends Number>
    void count(T table, EvalT defaultVal, String[] args, String expression, final Set<String> names, final Set<String> outNames) throws ParserException {
        CommonExpression<T, EvalT> expr =
            new ExpressionParser<T, EvalT>(table).parse(
                expression,
                names,
                outNames
            );
        System.out.println("Got: " + expr.toString());
        EvalT
            val0 = defaultVal,
            val1 = defaultVal,
            val2 = defaultVal;
        getNumbers(args, 2, table::parseNumber, new Number[]{val0, val1, val2}, outNames);
        System.out.println(expr.evaluate(val0, val1, val2));
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

            switch (args[0]) {
                case "-i":
                    count(
                        OperableIntTable.getInstance(), 0,
                        args, expression,
                        names, outNames
                    );
                    break;
                case "-d":
                    count(
                        OperableDoubleTable.getInstance(), 0.0,
                        args, expression,
                        names, outNames
                    );
                    break;
                case "-bi":
                    count(
                        OperableBigIntTable.getInstance(), BigInteger.ZERO,
                        args, expression,
                        names, outNames
                    );
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
