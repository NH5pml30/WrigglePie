package expression.parser;

import expression.operation.BinaryOperationTableEntry;
import expression.operation.UnaryOperationTableEntry;

import java.util.function.Function;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 * @author Nikolai Kholiavin
 */
public class StringSource extends ExpressionSource {
    private final String data;
    private int pos;
    private TokenType cachedTokenType;
    private TokenData cachedData = null;

    private enum State {
        PRE, POST, END
    }

    private State state = State.PRE;

    public StringSource( final String data ) {
        this.data = data;
    }

    private void skipWhitespaces() {
        while (pos < data.length() && Character.isWhitespace(data.charAt(pos))) {
            pos++;
        }
    }

    private boolean isStartNum( char ch ) {
        return ch >= '0' && ch <= '9' || ch == '-';
    }

    private boolean isPartNum( char ch ) {
        return ch >= '0' && ch <= '9';
    }

    private boolean isStartName( char ch ) {
        return Character.isAlphabetic(ch) || ch == '_';
    }

    private boolean isPartName( char ch ) {
        return Character.isAlphabetic(ch) || ch >= '0' && ch <= '9' || ch == '_';
    }

    private String read( Function<Character, Boolean> isStart, Function<Character, Boolean> isPart, Function<String, Boolean> isOk ) {
        if (!isStart.apply(data.charAt(pos))) {
            return "";
        }

        int end;
        for (end = pos + 1; end < data.length() && isPart.apply(data.charAt(end)); end++) {
        }
        String res = data.substring(pos, end);
        if (isOk.apply(res)) {
            return res;
        }
        return "";
    }

    private void cacheName( String name ) {
        testState(State.PRE, "variable name '" + name + "'");
        state = State.POST;
        cachedData = new TokenData(name);
        cachedTokenType = TokenType.NAME;
        pos += name.length();
    }

    private void cacheBinaryOp( BinaryOperationTableEntry op ) {
        testState(State.POST, "binary operation '" + op.getSymbol() + "'");
        state = State.PRE;
        cachedData = new TokenData(op);
        cachedTokenType = TokenType.BINARY_OP;
        pos += op.getSymbol().length();
    }

    private void cacheUnaryOp( UnaryOperationTableEntry op ) {
        testState(State.PRE, "unary operation '" + op.getSymbol() + "'");
        cachedData = new TokenData(op);
        cachedTokenType = TokenType.UNARY_OP;
        pos += op.getSymbol().length();
    }

    private boolean cacheNumber( String strVal ) {
        testState(State.PRE, "number");
        state = State.POST;
        try {
            cachedData = new TokenData(Integer.parseInt(strVal));
        } catch ( NumberFormatException e ) {
            throw error("number too long for 32-bit: " + strVal);
        }
        cachedTokenType = TokenType.NUMBER;
        pos += strVal.length();
        return true;
    }

    private void cacheNext() {
        if (cachedData != null || state == State.END) {
            return;
        }

        skipWhitespaces();
        if (pos == data.length()) {
            testState(State.POST, "end of expression");
            state = State.END;
            cachedTokenType = TokenType.NONE;
            return;
        }


        String buf;
        if (!(buf = read(this::isPartNum, this::isPartNum, x -> true)).isEmpty()) {
            cacheNumber(buf);
        } else {
            switch (data.charAt(pos)) {
                case '(':
                    testState(State.PRE, "(");
                    cachedTokenType = TokenType.LEFT_PAR;
                    pos++;
                    break;
                case ')':
                    testState(State.POST, ")");
                    cachedTokenType = TokenType.RIGHT_PAR;
                    pos++;
                    break;
                case '+':
                    cacheBinaryOp(BinaryOperationTableEntry.ADD);
                    break;
                case '-':
                    if (state == State.PRE) {
                        if (!(buf = read(this::isStartNum, this::isPartNum, x -> !x.equals("-"))).isEmpty()) {
                            cacheNumber(buf);
                        } else {
                            cacheUnaryOp(UnaryOperationTableEntry.UNARY_MINUS);
                        }
                    } else {
                        cacheBinaryOp(BinaryOperationTableEntry.SUBTRACT);
                    }
                    break;
                case '*':
                    cacheBinaryOp(BinaryOperationTableEntry.MULTIPLY);
                    break;
                case '/':
                    cacheBinaryOp(BinaryOperationTableEntry.DIVIDE);
                    break;
                default:
                    String name = read(this::isStartName, this::isPartName, x -> true);
                    if (name.isEmpty()) {
                        throw error("unsupported token part: '" + data.charAt(pos) + "'");
                    }
                    cacheName(name);
            }
        }
    }

    private void expectNext( char ch ) {
        if (pos + 1 >= data.length()) {
            throw error("expected '" + ch + "' , got end of expression");
        }
        if (data.charAt(pos + 1) != ch) {
            throw error("expected '" + ch + "' , got '" + data.charAt(pos + 1) + "'");
        }
    }

    @Override
    public boolean hasNext() {
        cacheNext();
        return cachedData != null;
    }

    @Override
    public TokenType next() {
        cacheNext();
        tokenData = cachedData;
        cachedData = null;
        return cachedTokenType;
    }

    private void testState( State expected, String got ) {
        if (state != expected) {
            String message = "expected ";

            switch (state) {
                case PRE:
                    message += "'(', unary operation, number or variable";
                    break;
                case POST:
                    message += "')', binary operation or end of expression";
                    break;
            }
            message += ", got " + got;
            throw error(message);
        }
    }

    @Override
    public ParserException error( final String message ) {
        return new ParserException(pos + ": " + message);
    }
}
