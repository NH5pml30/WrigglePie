import java.io.*;
import java.nio.charset.Charset;
import java.util.*;

class Counter {
    private int val = 0;

    Counter() {
    }

    Counter(int val) {
        this.val = val;
    }

    int increment() {
        return ++val;
    }

    int decrement() {
        return --val;
    }

    int postIncrement() {
        return val++;
    }

    int postDecrement() {
        return val--;
    }

    int get() {
        return val;
    }

    void set(int newVal) {
        val = newVal;
    }
}

public class FastScanner implements AutoCloseable {
    private enum State {
        OPENED,
        CLOSED,
        REACHED_END
    }
    private State state = State.CLOSED;

    public interface DelimiterFunction {
        boolean apply( char character );
    }
    private DelimiterFunction tokenDelimeter = Character::isWhitespace;

    private Reader reader;

    private static class Token {
        String text;
        int linesSkippedBefore;

        Token(String text, int linesSkippedBefore) {
            this.text = text;
            this.linesSkippedBefore = linesSkippedBefore;
        }
    }
    private Deque<Token> cachedTokens = new ArrayDeque<>();
    private int cachedSkippedLines = 0;
    private final int NEXT_LINE_NOT_CACHED = -2;
    private final int NEXT_LINE_NOT_PRESENT = -1;
    private int tokensUntilNextLine = NEXT_LINE_NOT_CACHED;

    private int cachedInt;
    private boolean isIntCached = false;
    private int cachedIntRadix;

    private FastScanner( Reader reader ) {
        this.reader = new BufferedReader(reader);
        this.state = State.OPENED;
    }

    private FastScanner( Reader reader, DelimiterFunction tokenDelimeter ) {
        this.reader = new BufferedReader(reader);
        this.tokenDelimeter = x -> x == '\n' || tokenDelimeter.apply(x);
        this.state = State.OPENED;
    }

    public FastScanner( InputStream source ) {
        this(new InputStreamReader(source));
    }

    public FastScanner( InputStream source, DelimiterFunction tokenDelimeter ) {
        this(new InputStreamReader(source), tokenDelimeter);
    }

    public FastScanner( InputStream source, Charset cs ) {
        this(new InputStreamReader(source, cs));
    }

    public FastScanner( InputStream source, Charset cs, DelimiterFunction tokenDelimeter ) {
        this(new InputStreamReader(source, cs), tokenDelimeter);
    }

    private int getChar() {
        int ch;
        try {
            if ((ch = reader.read()) == -1) {
                state = State.REACHED_END;
            }
        } catch ( IOException e ) {
            state = State.REACHED_END;
            ch = -1;
        }
        return ch;
    }

    private boolean hasChar() {
        try {
            return reader.ready();
        } catch ( IOException e ) {
            return false;
        }
    }

    private boolean readToken() throws IllegalStateException {
        switch (state) {
            case CLOSED:
                throw new IllegalStateException("Scanner has been closed");
            case REACHED_END:
                return false;
            default: // OPENED
                int ch;
                StringBuilder token = new StringBuilder();
                int
                    skippedLinesBefore = 0,
                    skippedLinesAfter = 0;
                boolean isLastLineSeparator = false;

                do {
                    ch = getChar();
                    if (ch != -1) {
                        isLastLineSeparator = (ch == '\n');

                        if (!tokenDelimeter.apply((char)ch)) {
                            token.append((char)ch);
                        } else {
                            if (token.length() == 0) {
                                if (ch == '\n') {
                                    skippedLinesBefore++;
                                }
                            } else {
                                if (ch == '\n') {
                                    skippedLinesAfter++;
                                }
                                break;
                            }
                        }
                    }
                } while (ch != -1);

                int skippedLinesCorrection = 0;
                if (!hasChar() && isLastLineSeparator) {
                    skippedLinesCorrection = -1;
                }

                if (token.length() != 0) {
                    cachedTokens.add(new Token(token.toString(),
                        cachedSkippedLines + skippedLinesBefore));
                    cachedSkippedLines = skippedLinesAfter + skippedLinesCorrection;
                    return true;
                }
                cachedSkippedLines += skippedLinesBefore + skippedLinesCorrection;
                return false;
        }
    }

    public boolean hasNext() throws IllegalStateException {
        return !cachedTokens.isEmpty() || readToken();
    }

    public boolean hasNextInLine() throws IllegalStateException {
        if (cachedTokens.isEmpty()) {
            if (cachedSkippedLines != 0) {
                return false;
            }
            readToken();
        }
        return !cachedTokens.isEmpty() && cachedTokens.peek().linesSkippedBefore == 0;
    }

    public boolean hasNextInt( int radix ) throws IllegalStateException {
        if (!hasNext()) {
            return false;
        }

        if (isIntCached && cachedIntRadix == radix) {
            return true;
        }
        isIntCached = false;
        try {
            cachedInt = Integer.parseInt(cachedTokens.peek().text);
            cachedIntRadix = radix;
            isIntCached = true;
        } catch ( NumberFormatException e ) {
            return false;
        }
        return true;
    }

    public boolean hasNextInt() throws IllegalStateException {
        return hasNextInt(10);
    }

    public boolean hasNextIntInLine( int radix ) throws IllegalStateException {
        return hasNextInLine() && hasNextInt(radix);
    }

    public boolean hasNextIntInLine() throws IllegalStateException {
        return hasNextIntInLine(10);
    }

    public String next() throws IllegalStateException, NoSuchElementException {
        if (!hasNext()) {
            throw new NoSuchElementException("No tokens left!");
        }
        String res = cachedTokens.remove().text;
        isIntCached = false;
        if (tokensUntilNextLine != NEXT_LINE_NOT_CACHED &&
                tokensUntilNextLine != NEXT_LINE_NOT_PRESENT) {
            if (tokensUntilNextLine != 0) {
                tokensUntilNextLine--;
            } else {
                tokensUntilNextLine = NEXT_LINE_NOT_CACHED;
            }
        }
        return res;
    }

    public boolean hasNextLine() throws IllegalStateException {
        if (tokensUntilNextLine != NEXT_LINE_NOT_CACHED) {
            return tokensUntilNextLine != NEXT_LINE_NOT_PRESENT;
        }

        tokensUntilNextLine = 0;
        for (Token token : cachedTokens) {
            if (token.linesSkippedBefore != 0) {
                return true;
            }
            tokensUntilNextLine++;
        }
        if (cachedSkippedLines != 0) {
            return true;
        }

        if (state == State.REACHED_END) {
            tokensUntilNextLine = NEXT_LINE_NOT_PRESENT;
            return false;
        }

        while (readToken()) {
            Token token = cachedTokens.getLast();
            if (token.linesSkippedBefore != 0) {
                return true;
            }
            tokensUntilNextLine++;
        }
        if (cachedSkippedLines != 0) {
            return true;
        }

        tokensUntilNextLine = NEXT_LINE_NOT_PRESENT;
        return false;
    }

    public void skipLine() throws IllegalStateException, NoSuchElementException {
        if (!hasNextLine()) {
            if (cachedTokens.isEmpty()) {
                throw new NoSuchElementException("No lines left!");
            }
            cachedTokens.clear();
            return;
        }

        if (tokensUntilNextLine != 0) {
            for (int i = 0; i < tokensUntilNextLine; i++) {
                cachedTokens.remove();
            }
        } else {
            if (cachedTokens.isEmpty()) {
                cachedSkippedLines--;
            } else {
                cachedTokens.peek().linesSkippedBefore--;
            }
        }
        tokensUntilNextLine = NEXT_LINE_NOT_CACHED;
    }

    public int nextInt( int radix ) throws IllegalStateException, NoSuchElementException {
        if (!hasNextInt(radix)) {
            throw new NoSuchElementException("No ints in " + radix + " radix left!");
        }
        next();
        return cachedInt;
    }

    public int nextInt() throws IllegalStateException, NoSuchElementException {
        return nextInt(10);
    }

    @Override
    public void close() throws IOException {
        state = State.CLOSED;
        reader.close();
    }
}
