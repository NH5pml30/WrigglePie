package expression.parser;

import expression.operation.BinaryOperationTableEntry;
import expression.operation.UnaryOperationTableEntry;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 * @author Nikolai Kholiavin
 */
public abstract class ExpressionSource {
    enum TokenType {
        LEFT_PAR("("),
        RIGHT_PAR(")"),
        NUMBER("number"),
        BINARY_OP("binary operation"),
        UNARY_OP("unary operation"),
        NAME("variable name"),
        NONE("end of expression");

        public final String represent;

        TokenType(String s) {
            this.represent = s;
        }
    }

    static final class TokenData {
        final String str;
        final BinaryOperationTableEntry bOp;
        final UnaryOperationTableEntry uOp;

        TokenData(String str) {
            this.str = str;
            this.bOp = null;
            this.uOp = null;
        }

        TokenData(BinaryOperationTableEntry bOp) {
            this.str = null;
            this.bOp = bOp;
            this.uOp = null;
        }

        TokenData(UnaryOperationTableEntry uOp) {
            this.str = null;
            this.bOp = null;
            this.uOp = uOp;
        }
    }

    TokenData tokenData;

    abstract TokenType next() throws ParserException;

    abstract ParserException error(final String message);
}
