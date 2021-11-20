package info.kgeorgiy.ja.holyavin.bank;

import org.junit.*;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.junit.runners.MethodSorters;

import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.util.Collections;
import java.util.concurrent.*;
import java.util.function.Consumer;

@RunWith(JUnit4.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class BankTest {
    private static final int DEFAULT_PORT = 8888;
    private Process serverProcess;
    private static Process registryProcess;
    public static final String CUT_PROPERTY = "cut";
    protected String testMethodName;
    private static final String BANK_URL = "//localhost/bank";

    @Rule
    public TestRule watcher = watcher(description -> {
        testMethodName = description.getMethodName();
        System.err.println("=== Running " + testMethodName);
    });

    protected static TestWatcher watcher(final Consumer<Description> watcher) {
        return new TestWatcher() {
            @Override
            protected void starting(final Description description) {
                watcher.accept(description);
            }
        };
    }

    private static String getClassPath(Class<?> token) {
        try {
            return Path.of(token.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        } catch (final URISyntaxException e) {
            throw new AssertionError(e);
        }
    }

    public static Class<?> loadClass() {
        final String className = System.getProperty(CUT_PROPERTY);
        Assert.assertNotNull("Class name not specified", className);

        try {
            return Class.forName(className);
        } catch (final ClassNotFoundException e) {
            throw new AssertionError(e);
        }
    }

    @BeforeClass
    public static void beforeAll() {
        registryProcess = startProcess(new String[] { "rmiregistry", "-J--class-path=" +
                getClassPath(loadClass()) });
    }

    private static Process startProcess(String[] cmd) {
        Process process;
        try {
            process = new ProcessBuilder(cmd).inheritIO().start();
        } catch (final IOException e) {
            throw new AssertionError(e);
        }
        return process;
    }

    private static void destroyProcess(Process process) {
        if (process == null) {
            return;
        }
        process.destroy();
        try {
            process.waitFor(1, TimeUnit.SECONDS);
            Assert.assertFalse("Cannot stop server process after 1s", process.isAlive());
        } catch (final InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    @AfterClass
    public static void afterAll() {
        destroyProcess(registryProcess);
    }

    @Before
    public void beforeEach() {
        Assert.assertNull("Server already started", serverProcess);
        try {
            Naming.unbind(BANK_URL);
        } catch (RemoteException | MalformedURLException e) {
            throw new AssertionError(e);
        } catch (NotBoundException ignored) {
        }
        serverProcess = startProcess(new String[]{ "java", "-cp", getClassPath(loadClass()),
                Server.class.getCanonicalName(), String.valueOf(DEFAULT_PORT) });
        try {
            while (true) {
                try {
                    Naming.lookup(BANK_URL);
                    break;
                } catch (RemoteException | NotBoundException ignored) {
                } catch (MalformedURLException e) {
                    throw new AssertionError(e);
                }
                serverProcess.waitFor(100, TimeUnit.MILLISECONDS);
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            Assert.fail("Interrupted");
        }
    }

    @After
    public void afterEach() {
        destroyProcess(serverProcess);
        serverProcess = null;
    }

    @Test
    public void test01_constructors() {
        final Class<?> token = loadClass();
        Assert.assertTrue(token.getName() + " should implement MoneyGenerator interface",
                MoneyGenerator.class.isAssignableFrom(token));
        try {
            token.getConstructor();
        } catch (final NoSuchMethodException e) {
            Assert.fail(token.getName() + " should have a default constructor");
        }
    }

    interface AppTestFunction {
        void test(MoneyGenerator app) throws MoneyGeneratorException;
    }

    protected static MoneyGenerator createClient() {
        final Class<?> token = loadClass();
        MoneyGenerator client;
        try {
            client = (MoneyGenerator) token.getConstructor().newInstance();
        } catch (InstantiationException | NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
            throw new AssertionError(e);
        }
        return client;
    }

    protected static void testApp(AppTestFunction test) {
        try {
            test.test(createClient());
        } catch (MoneyGeneratorException e) {
            e.printStackTrace();
            Assert.fail("No error expected");
        }
    }

    protected void testMt(int threads, Runnable runnable) {
        Callable<Void> callable = () -> {
            runnable.run();
            return null;
        };

        ExecutorService executor = Executors.newFixedThreadPool(threads);
        try {
            for (var future : executor.invokeAll(Collections.nCopies(threads, callable))) {
                try {
                    future.get();
                } catch (ExecutionException e) {
                    if (e.getCause() instanceof AssertionError) {
                        AssertionError error = (AssertionError) e.getCause();
                        Assert.fail(error.toString());
                    } else {
                        throw new AssertionError(e);
                    }
                }
            }
            executor.shutdown();
            if (!executor.awaitTermination(1, TimeUnit.SECONDS)) {
                Assert.fail("Executor service failed to exit after 1 second");
            }
        } catch (InterruptedException e) {
            throw new AssertionError(e);
        }
    }

    protected void testAppMt(int threads, AppTestFunction test, AppTestFunction after) {
        testMt(threads, () -> testApp(test));
        testApp(after);
    }

    @Test
    public void test02_simple() {
        testApp(client -> {
            Assert.assertEquals(100, client.giveMoney("Name", "Surname", 123, "checking", 100));
            Assert.assertEquals(200, client.giveMoney("Name", "Surname", 123, "checking", 100));
        });
    }

    @Test
    public void test03_collision() {
        testApp(client -> {
            Assert.assertEquals(100, client.giveMoney("Name", "Surname", 123, "checking", 100));
            try {
                client.giveMoney("🤡", "Surname", 123, "checking", 100);
            } catch (MoneyGeneratorException ignored) {
                // ok
                return;
            }
            Assert.fail("Error expected");
        });
    }

    @Test
    public void test04_modificationPropagation() {
        testApp(client -> {
            try {
                startProcess(new String[] { "java", "-cp", getClassPath(loadClass()), loadClass().getCanonicalName(),
                        "Name", "Surname", "123", "checking", "100" }).waitFor();
            } catch (InterruptedException e) {
                throw new AssertionError(e);
            }

            Assert.assertEquals(200, client.giveMoney("Name", "Surname", 123, "checking", 100));
        });
    }

    @Test
    public void test05_multithreaded() {
        String name = "🤡", surname = "🤡ov", account = "checking";
        int passport = 1001, amount = 123, threads = 10;

        testAppMt(threads,
                client -> client.giveMoney(name, surname, passport, account, amount),
                client -> Assert.assertEquals(
                                amount * threads,
                                client.giveMoney(name, surname, passport, account, 0)
                ));
    }

    interface BankTestFunction {
        void test(Bank bank) throws RemoteException;
    }

    protected void testBank(BankTestFunction test) {
        final Bank bank;
        try {
            bank = (Bank) Naming.lookup(BANK_URL);
            test.test(bank);
        } catch (final NotBoundException | MalformedURLException | RemoteException e) {
            throw new AssertionError(e);
        }
    }

    protected void testBankMt(int threads, BankTestFunction test, BankTestFunction after) {
        testMt(threads, () -> testBank(test));
        testBank(after);
    }

    @Test
    public void test06_bankSimple() {
        testBank(bank -> {
            Person person = bank.createPerson("Name", "Surname", 123);
            Account account = person.createAccount("checking");
            account.setAmount(100);
            Assert.assertEquals(100, account.getAmount());
            account.setAmount(200);
            Assert.assertEquals(200, account.getAmount());
            account = person.createAccount("savings");
            account.setAmount(10000);
            Assert.assertEquals(10000, account.getAmount());
        });
    }

    @Test
    public void test07_bankLocalRemote() {
        testBank(bank -> {
            Person person = bank.createPerson("Name", "Surname", 123);
            Person person2 = bank.getPerson(123);
            LocalPerson localPerson = bank.getLocalPerson(123);
            Account account = person.createAccount("checking");
            LocalPerson localPerson2 = bank.getLocalPerson(123);
            account.setAmount(100);
            Assert.assertEquals("Global changes not propagated",
                    100, person2.getAccount("checking").getAmount());
            Assert.assertNull("Global changes propagated to older local",
                    localPerson.getAccount("checking"));
            Assert.assertEquals("Global changes propagated to older local",
                    0, localPerson2.getAccount("checking").getAmount());

            Account localAccount = localPerson2.getAccount("checking");
            Assert.assertNotNull("Global changes not applied", localAccount);
            localAccount.setAmount(10000);
            Assert.assertEquals("Local changes propagated", 100, person.getAccount("checking").getAmount());
            localPerson2.createAccount("savings");
            Assert.assertNull("Local changes propagated", person2.getAccount("savings"));
        });
    }

    @Test
    public void test08_bankAccountId() {
        testBank(bank -> {
            Person person = bank.createPerson("Name", "Surname", 123);
            person.createAccount("checking");
            Assert.assertNotNull("Wrong account id", bank.getAccount("123:checking"));

            LocalPerson localPerson = bank.getLocalPerson(123);
            localPerson.createAccount("savings");
            Assert.assertNull("Local account propagated to bank", bank.getAccount("123:savings"));
        });
    }

    @Test
    public void test09_bankMultithreaded() {
        String name = "Name", surname = "Surname", account ="checking";
        int passport = 1001, amount = 100, threads = 10;

        testBankMt(10,
                bank -> {
                    Person person = bank.createPerson(name, surname, passport);
                    Account acc = person.createAccount(account);
                    acc.addAmount(amount);
                },
                bank ->
                        Assert.assertEquals(threads * amount,
                                bank.createPerson(name, surname, passport).createAccount(account).getAmount())
        );
    }
}
