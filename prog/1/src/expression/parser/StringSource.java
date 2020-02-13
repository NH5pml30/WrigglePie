package expression.parser;

import expression.Const;
import expression.operation.BinaryOperationTableEntry;
import expression.operation.UnaryOperationTableEntry;

import java.util.Set;
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
    private Set<String> varNames = null;

    private enum State {
        PRE, POST, END
    }

    private State state = State.PRE;

    public StringSource( final String data ) {
        this.data = data;
    }

    public StringSource( final String data, final Set<String> varNames ) {
        this(data);
        this.varNames = varNames;
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
            cachedData = new TokenData(new Const(strVal));
        } catch ( NumberFormatException e ) {
            throw error("number too long: " + strVal);
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
                    if (pos + 1 < data.length() && data.charAt(pos + 1) == '*') {
                        cacheBinaryOp(BinaryOperationTableEntry.POW);
                    } else {
                        cacheBinaryOp(BinaryOperationTableEntry.MULTIPLY);
                    }
                    break;
                case '/':
                    if (pos + 1 < data.length() && data.charAt(pos + 1) == '/') {
                        cacheBinaryOp(BinaryOperationTableEntry.LOG);
                    } else {
                        cacheBinaryOp(BinaryOperationTableEntry.DIVIDE);
                    }
                    break;
                default:
                    String name = read(this::isStartName, this::isPartName, x -> true);
                    if (name.isEmpty()) {
                        throw error("unsupported token part: '" + data.charAt(pos) + "'");
                    }
                    switch (name) {
                        case "log2":
                            cacheUnaryOp(UnaryOperationTableEntry.LOG2);
                            break;
                        case "pow2":
                            cacheUnaryOp(UnaryOperationTableEntry.POW2);
                            break;
                        default:
                            if (varNames != null && !varNames.contains(name)) {
                                throw error("variable/function name '" + name + "' not supported!");
                            }
                            cacheName(name);
                    }
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

    private String getContext( int radius ) {
        return
            (pos - radius > 0 ? ".." : "") +
            data.substring(Integer.max(pos - radius, 0), pos) + "|" +
            data.substring(pos, Integer.min(pos + radius, data.length())) +
            (pos + radius < data.length() - 1 ? ".." : "");
    }

    @Override
    public ParserException error( final String message ) {
        return new ParserException(
            pos + " (" + getContext(5) + "): " + message
        );
    }
}
