package expression;

public class ExpressionMain {
    public static void main( String[] args ) {
        int x = Integer.parseInt(args[0]);
        Variable varX = new Variable("x");
        CommonExpression expression = new CheckedAdd(
            varX,
            new CheckedAdd(
                new Const(2),
                varX)
        );
        System.out.println(expression.evaluate(x));
        System.out.println(expression.toMiniString());
    }
}
