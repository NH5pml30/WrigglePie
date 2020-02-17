package expression.parser;

import expression.Const;
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
        final Object data;

        private TokenData( Object data ) {
            this.data = data;
        }

        TokenData( Const val ) {
            this((Object)val);
        }

        TokenData( String name ) {
            this((Object)name);
        }

        TokenData( BinaryOperationTableEntry op ) {
            this((Object)op);
        }

        TokenData( UnaryOperationTableEntry op ) {
            this((Object)op);
        }

        @SuppressWarnings("unchecked")
        <T> T get() {
            return (T)data;
        }
    }

    TokenData tokenData;

    abstract boolean hasNext();

    abstract TokenType next();

    abstract ParserException error( final String message );
}
