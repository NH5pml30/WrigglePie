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

        TokenType( String s ) {
            this.represent = s;
        }
    }

    static final class TokenData {
        final Integer val;
        final String name;
        final BinaryOperationTableEntry biOp;
        final UnaryOperationTableEntry unOp;

        TokenData( int val ) {
            this.val = val;
            this.name = null;
            this.biOp = null;
            this.unOp = null;
        }

        TokenData( String name ) {
            this.val = null;
            this.name = name;
            this.biOp = null;
            this.unOp = null;
        }

        TokenData( BinaryOperationTableEntry op ) {
            this.val = null;
            this.name = null;
            this.biOp = op;
            this.unOp = null;
        }

        TokenData( UnaryOperationTableEntry op ) {
            this.val = null;
            this.name = null;
            this.biOp = null;
            this.unOp = op;
        }
    }

    TokenData tokenData;

    abstract boolean hasNext();

    abstract TokenType next();

    abstract ParserException error( final String message );
}
