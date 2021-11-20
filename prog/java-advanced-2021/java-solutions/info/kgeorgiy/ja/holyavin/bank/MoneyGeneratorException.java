package info.kgeorgiy.ja.holyavin.bank;

public class MoneyGeneratorException extends Exception {
    MoneyGeneratorException(String message) {
        super(message);
    }

    MoneyGeneratorException(String message, Throwable cause) {
        super(message, cause);
    }
}
