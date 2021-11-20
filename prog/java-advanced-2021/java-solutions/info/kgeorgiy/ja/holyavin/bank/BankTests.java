package info.kgeorgiy.ja.holyavin.bank;

import org.junit.runner.JUnitCore;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;

public class BankTests {
    private final long start = System.currentTimeMillis();

    public static void main(final String... args) {
        new BankTests().run(args);
    }

    public void run(final String[] args) {
        if (args.length != 1) {
            System.out.println("Usage: <tester> <testee>");
            return;
        }

        final String cut = args[0];
        test(cut, BankTest.class);

        System.out.println("============================");
        final long time = System.currentTimeMillis() - start;
        System.out.printf("OK %s for %s in %dms %n", BankTest.class.getName(), cut, time);
    }

    private static void test(final String cut, final Class<?> test) {
        System.err.printf("Running %s for %s%n", test, cut);

        System.setProperty(BankTest.CUT_PROPERTY, cut);
        final Result result = new JUnitCore().run(test);
        if (result.wasSuccessful()) {
            return;
        }

        for (final Failure failure : result.getFailures()) {
            System.err.println("Test " + failure.getDescription().getMethodName() + " failed: " + failure.getMessage());
            if (failure.getException() != null) {
                failure.getException().printStackTrace();
            }
        }
        System.exit(1);
        throw new AssertionError("Exit");
    }
}
