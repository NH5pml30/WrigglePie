package expression.parser;

import expression.operation.BinaryOperation;
import expression.operation.BinaryOperationTableEntry;
import expression.operation.UnaryOperationTableEntry;
import expression.parser.exception.ParserException;
import expression.parser.exception.ParserExceptionCreator;
import expression.parser.exception.UnexpectedTokenException;
import expression.parser.exception.UnrecognizedTokenException;

import java.util.HashMap;
import java.util.Map;
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


    private static final Map<String, BinaryOperationTableEntry> binaryOperators = new HashMap<>();
    private static final Map<String, UnaryOperationTableEntry> unaryOperators = new HashMap<>();

    static {
        for (BinaryOperationTableEntry entry : BinaryOperationTableEntry.values()) {
            binaryOperators.put(entry.getSymbol(), entry);
        }
        for (UnaryOperationTableEntry entry : UnaryOperationTableEntry.values()) {
            unaryOperators.put(entry.getSymbol(), entry);
        }
    }

    public StringSource(final String data) {
        this.data = data;
    }

    public StringSource(final String data, final Set<String> varNames) {
        this(data);
        this.varNames = varNames;
    }

    private void skipWhitespaces() {
        while (pos < data.length() && Character.isWhitespace(data.charAt(pos))) {
            pos++;
        }
    }

    private boolean isStartNum(char ch) {
        return ch >= '0' && ch <= '9' || ch == '-';
    }

    private boolean isPartNum(char ch) {
        return ch >= '0' && ch <= '9';
    }

    private boolean isStartName(char ch) {
        return Character.isAlphabetic(ch) || ch == '_';
    }

    private boolean isPartName(char ch) {
        return Character.isAlphabetic(ch) || ch >= '0' && ch <= '9' || ch == '_';
    }

    private String read(Function<Character, Boolean> isStart, Function<Character, Boolean> isPart, Function<String, Boolean> isOk) {
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

    private String readToken() {
        return read(x -> true, x -> !Character.isWhitespace(x), x -> true);
    }

    private void checkName(String name) throws ParserException {
        if (!isStartName(name.charAt(0))) {
            throw error(UnrecognizedTokenException::new, "unsupported token start for a name: " + name);
        }
        for (int i = 1; i < name.length(); i++) {
            if (!isPartName(name.charAt(i))) {
                throw error(UnrecognizedTokenException::new, "unsupported token part for a name: " + name);
            }
        }
    }

    private void cacheName(String name) throws ParserException {
        testState(State.PRE, "variable name '" + name + "'");
        state = State.POST;
        cachedData = new TokenData(name);
        cachedTokenType = TokenType.NAME;
        pos += name.length();
    }

    private void cacheBinaryOp(BinaryOperationTableEntry op) throws ParserException {
        testState(State.POST, "binary operation '" + op.getSymbol() + "'");
        state = State.PRE;
        cachedData = new TokenData(op);
        cachedTokenType = TokenType.BINARY_OP;
        pos += op.getSymbol().length();
    }

    private void cacheUnaryOp(UnaryOperationTableEntry op) throws ParserException {
        testState(State.PRE, "unary operation '" + op.getSymbol() + "'");
        cachedData = new TokenData(op);
        cachedTokenType = TokenType.UNARY_OP;
        pos += op.getSymbol().length();
    }

    private void cacheNumber(String strVal) throws ParserException {
        testState(State.PRE, "number");
        state = State.POST;
        cachedData = new TokenData(strVal);
        cachedTokenType = TokenType.NUMBER;
        pos += strVal.length();
    }

    private void cacheNext() throws ParserException {
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
                default:
                    switch (state) {
                        case PRE:
                            if (
                                data.charAt(pos) == '-' &&
                                !(buf = read(this::isStartNum, this::isPartNum, x -> !x.equals("-"))).isEmpty()
                            ) {
                                cacheNumber(buf);
                            } else {
                                String token = readToken();
                                UnaryOperationTableEntry op = unaryOperators.get(token);
                                if (op == null) {
                                    checkName(token);
                                    if (varNames != null && !varNames.contains(token)) {
                                        throw error(
                                            UnrecognizedTokenException::new,
                                            "variable/function name '" + token + "' not supported!"
                                        );
                                    }
                                    cacheName(token);
                                } else {
                                    cacheUnaryOp(op);
                                }
                            }
                            break;
                        case POST:
                            String token = readToken();
                            BinaryOperationTableEntry op = binaryOperators.get(token);
                            if (op == null) {
                                throw error(UnrecognizedTokenException::new, token);
                            }
                            cacheBinaryOp(op);
                    }
                    break;
            }
        }
    }

    @Override
    public TokenType next() throws ParserException {
        cacheNext();
        tokenData = cachedData;
        cachedData = null;
        return cachedTokenType;
    }

    private void testState(State expected, String got) throws ParserException {
        if (state != expected) {
            String message = got + ", expected ";

            switch (state) {
                case PRE:
                    message += "'(', unary operation, number or variable";
                    break;
                case POST:
                    message += "')', binary operation or end of expression";
                    break;
            }
            throw error(UnexpectedTokenException::new, message);
        }
    }

    private String getContext(int radius) {
        return
            (pos - radius > 0 ? ".." : "") +
                data.substring(Integer.max(pos - radius, 0), pos) + "|" +
                data.substring(pos, Integer.min(pos + radius, data.length())) +
                (pos + radius < data.length() - 1 ? ".." : "");
    }

    @Override
    public ParserException error(ParserExceptionCreator factory, final String message) {
        return factory.create(
            pos, getContext(5), message
        );
    }
}
