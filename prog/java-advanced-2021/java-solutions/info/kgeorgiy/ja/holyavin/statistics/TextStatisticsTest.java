package info.kgeorgiy.ja.holyavin.statistics;

import org.junit.*;
import org.junit.rules.TestName;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.junit.runners.MethodSorters;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.text.*;
import java.time.LocalDate;
import java.time.Month;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;


@RunWith(JUnit4.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TextStatisticsTest {
    public static final String CUT_PROPERTY = "cut";
    public static final String TEST_DIR = "__test__";
    public static final Locale RU_LOCALE = Locale.forLanguageTag("ru-RU");
    protected String testMethodName;

    @Rule
    public TestRule watcher = watcher(description -> {
        testMethodName = description.getMethodName();
        System.err.println("=== Running " + testMethodName);
    });

    @Rule public TestName name = new TestName();

    protected static TestWatcher watcher(final Consumer<Description> watcher) {
        return new TestWatcher() {
            @Override
            protected void starting(final Description description) {
                watcher.accept(description);
            }
        };
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

    @AfterClass
    public static void afterAll() {
        final FileVisitor<Path> visitor = new SimpleFileVisitor<>() {
            @Override
            public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
                Files.delete(file);
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFileFailed(final Path file, final IOException exc) throws IOException {
                Files.delete(file);
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult postVisitDirectory(final Path dir, final IOException exc) throws IOException {
                if (exc != null) {
                    throw exc;
                }
                Files.delete(dir);
                return FileVisitResult.CONTINUE;
            }
        };
        try {
            Files.walkFileTree(Path.of(TEST_DIR), visitor);
        } catch (final IOException e) {
            throw new AssertionError(e);
        }
    }

    @Test
    public void test01_constructors() {
        final Class<?> token = loadClass();
        Assert.assertTrue(token.getName() + " should implement TextStatistics interface",
                TextStatisticsReporter.class.isAssignableFrom(token));
        try {
            token.getConstructor();
        } catch (final NoSuchMethodException e) {
            Assert.fail(token.getName() + " should have a default constructor");
        }
    }

    interface AppTestFunction {
        void test(TextStatisticsReporter app) throws IOException;
    }

    protected static TextStatisticsReporter createProgram() {
        final Class<?> token = loadClass();
        final TextStatisticsReporter client;
        try {
            client = (TextStatisticsReporter) token.getConstructor().newInstance();
        } catch (final InstantiationException | NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
            throw new AssertionError(e);
        }
        return client;
    }

    protected static void testApp(final AppTestFunction test) {
        try {
            test.test(createProgram());
        } catch (final IOException e) {
            e.printStackTrace();
            Assert.fail("No error expected");
        }
    }

    protected void testAppText(final String text, final String fileName, final Locale inLocale,
                               final List<String> sentences, final List<String> words, final List<Double> numbers,
                               final List<Double> amounts, final List<LocalDate> dates) {
        testApp(app -> {
            Files.createDirectories(Path.of(TEST_DIR, name.getMethodName()));
            final Path path = Path.of(TEST_DIR, name.getMethodName(), fileName);
            Files.writeString(path, text);
            String report = app.generateReport(inLocale, Locale.US, path);
            Assert.assertEquals(generateReport(fileName, inLocale, false, sentences, words, numbers, amounts, dates), report);
            report = app.generateReport(inLocale, RU_LOCALE, path);
            Assert.assertEquals(generateReport(fileName, inLocale, true, sentences, words, numbers, amounts, dates), report);
        });
    }

    protected static long nDiff(final List<?> list) {
        return list.stream().distinct().count();
    }

    protected static String quotize(final String str) {
        return "\"" + str + "\"";
    }

    protected static <T extends Comparable<T>> String max(final Format format, final List<T> list) {
        return list.stream().max(Comparator.naturalOrder()).map(format::format).orElse("<None>");
    }
    protected static <T extends Comparable<T>> String min(final Format format, final List<T> list) {
        return list.stream().min(Comparator.naturalOrder()).map(format::format).orElse("<None>");
    }
    protected static String maxStr(final Locale locale, final List<String> list) {
        return list.stream().max(Collator.getInstance(locale)).map(TextStatisticsTest::quotize).orElse("<None>");
    }
    protected static String minStr(final Locale locale, final List<String> list) {
        return list.stream().min(Collator.getInstance(locale)).map(TextStatisticsTest::quotize).orElse("<None>");
    }
    protected static String avg(final Format format, final List<BigDecimal> list) {
        return list.stream().reduce(BigDecimal::add).map(n -> n.doubleValue() / list.size())
                .map(format::format).orElse("<None>");
    }
    protected static String avgDate(final Locale locale, final List<Date> list) {
        final Calendar calendar = Calendar.getInstance(locale);
        return list.isEmpty() ? "<None>" :
                DateFormat.getDateInstance(DateFormat.MEDIUM, locale).format(
                        Date.from(calendar.toInstant().plus(
                                (long) (list.stream().mapToDouble(
                                        date -> (double) ChronoUnit.DAYS.between(calendar.toInstant(), date.toInstant())
                                ).sum() / list.size()),
                                ChronoUnit.DAYS))
                );
    }
    protected static String argMaxLength(final List<String> list) {
        return list.stream().max(Comparator.comparingInt(String::length)).map(TextStatisticsTest::quotize)
                .orElse("<None>");
    }
    protected static String argMinLength(final List<String> list) {
        return list.stream().min(Comparator.comparingInt(String::length)).map(TextStatisticsTest::quotize)
                .orElse("<None>");
    }
    protected static String maxLength(final Format format, final List<String> list) {
        final var res = list.stream().mapToInt(String::length).max();
        return res.isPresent() ? format.format(res.getAsInt()) : "<None>";
    }
    protected static String minLength(final Format format, final List<String> list) {
        final var res = list.stream().mapToInt(String::length).min();
        return res.isPresent() ? format.format(res.getAsInt()) : "<None>";
    }
    protected static String avgLength(final Format format, final List<String> list) {
        final var res = list.stream().mapToInt(String::length).average();
        return res.isPresent() ? format.format(res.getAsDouble()) : "<None>";
    }

    protected static String generateReport_en_US(final String fileName, final Locale inLocale,
                                                 final List<String> sentences, final List<String> words, final List<BigDecimal> numbers,
                                                 final List<BigDecimal> amounts, final List<Date> dates) {
        final Locale locale = Locale.US;
        final var currency = DecimalFormat.getCurrencyInstance(locale);
        currency.setCurrency(Currency.getInstance(inLocale));
        final var number = DecimalFormat.getInstance(locale);
        final var date = DateFormat.getDateInstance(DateFormat.MEDIUM, locale);
        return String.format(locale,
                "Analyzed file %s%n" +
                        "Summary statistics%n" +
                        "\tNumber of sentences: %d (%d different).%n" +
                        "\tNumber of words: %d (%d different).%n" +
                        "\tNumber of amounts: %d (%d different).%n" +
                        "\tNumber of numbers: %d (%d different).%n" +
                        "\tNumber of dates: %d (%d different).%n" +
                        "Sentences statistics%n" +
                        "\tNumber of sentences: %d (%d different).%n" +
                        "\tMinimum sentence: %s.%n" +
                        "\tMaximum sentence: %s.%n" +
                        "\tMinimum length of a sentence: %s (%s).%n" +
                        "\tMaximum length of a sentence: %s (%s).%n" +
                        "\tAverage length of a sentence: %s.%n" +
                        "Words statistics%n" +
                        "\tNumber of words: %d (%d different).%n" +
                        "\tMinimum word: %s.%n" +
                        "\tMaximum word: %s.%n" +
                        "\tMinimum length of a word: %s (%s).%n" +
                        "\tMaximum length of a word: %s (%s).%n" +
                        "\tAverage length of a word: %s.%n" +
                        "Amounts of money statistics%n" +
                        "\tNumber of amounts: %d (%d different).%n" +
                        "\tMinimum amount: %s.%n" +
                        "\tMaximum amount: %s.%n" +
                        "\tAverage amount: %s.%n" +
                        "Numbers statistics%n" +
                        "\tNumber of numbers: %d (%d different).%n" +
                        "\tMinimum number: %s.%n" +
                        "\tMaximum number: %s.%n" +
                        "\tAverage number: %s.%n" +
                        "Dates statistics%n" +
                        "\tNumber of dates: %d (%d different).%n" +
                        "\tMinimum date: %s.%n" +
                        "\tMaximum date: %s.%n" +
                        "\tAverage date: %s.%n",
                fileName,
                sentences.size(), nDiff(sentences),
                words.size(), nDiff(words),
                amounts.size(), nDiff(amounts),
                numbers.size(), nDiff(numbers),
                dates.size(), nDiff(dates),

                sentences.size(), nDiff(sentences),
                minStr(inLocale, sentences), maxStr(inLocale, sentences),
                minLength(number, sentences), argMinLength(sentences),
                maxLength(number, sentences), argMaxLength(sentences),
                avgLength(number, sentences),

                words.size(), nDiff(words),
                minStr(inLocale, words), maxStr(inLocale, words),
                minLength(number, words), argMinLength(words),
                maxLength(number, words), argMaxLength(words),
                avgLength(number, words),

                amounts.size(), nDiff(amounts),
                min(currency, amounts), max(currency, amounts), avg(currency, amounts),

                numbers.size(), nDiff(numbers),
                min(number, numbers), max(number, numbers), avg(number, numbers),

                dates.size(), nDiff(dates),
                min(date, dates), max(date, dates), avgDate(locale, dates)
        );
    }

    protected static String generateReport_ru_RU(final String fileName, final Locale inLocale,
                                                 final List<String> sentences, final List<String> words, final List<BigDecimal> numbers,
                                                 final List<BigDecimal> amounts, final List<Date> dates) {
        final Locale locale = RU_LOCALE;
        final var currency = DecimalFormat.getCurrencyInstance(locale);
        currency.setCurrency(Currency.getInstance(inLocale));
        final var number = DecimalFormat.getInstance(locale);
        final var date = DateFormat.getDateInstance(DateFormat.MEDIUM, locale);
        return String.format(locale,
                "Анализируемый файл %s%n" +
                        "Сводная статистика%n" +
                        "\tЧисло предложений: %d (%d %s).%n" +
                        "\tЧисло слов: %s (%s %s).%n" +
                        "\tЧисло сумм: %s (%s %s).%n" +
                        "\tЧисло чисел: %s (%s %s).%n" +
                        "\tЧисло дат: %s (%s %s).%n" +
                        "Статистика по предложениям%n" +
                        "\tЧисло предложений: %d (%d %s).%n" +
                        "\tМинимальное предложение: %s.%n" +
                        "\tМаксимальное предложение: %s.%n" +
                        "\tМинимальная длина предложения: %s (%s).%n" +
                        "\tМаксимальная длина предложения: %s (%s).%n" +
                        "\tСредняя длина предложения: %s.%n" +
                        "Статистика по словам%n" +
                        "\tЧисло слов: %d (%d %s).%n" +
                        "\tМинимальное слово: %s.%n" +
                        "\tМаксимальное слово: %s.%n" +
                        "\tМинимальная длина слова: %s (%s).%n" +
                        "\tМаксимальная длина слова: %s (%s).%n" +
                        "\tСредняя длина слова: %s.%n" +
                        "Статистика по суммам денег%n" +
                        "\tЧисло сумм: %d (%d %s).%n" +
                        "\tМинимальная сумма: %s.%n" +
                        "\tМаксимальная сумма: %s.%n" +
                        "\tСредняя сумма: %s.%n" +
                        "Статистика по числам%n" +
                        "\tЧисло чисел: %d (%d %s).%n" +
                        "\tМинимальное число: %s.%n" +
                        "\tМаксимальное число: %s.%n" +
                        "\tСреднее число: %s.%n" +
                        "Статистика по датам%n" +
                        "\tЧисло дат: %d (%d %s).%n" +
                        "\tМинимальная дата: %s.%n" +
                        "\tМаксимальная дата: %s.%n" +
                        "\tСредняя дата: %s.%n",
                fileName,
                sentences.size(), nDiff(sentences), nDiff(sentences) == 1 ? "различное" : "различных",
                words.size(), nDiff(words), nDiff(words) == 1 ? "различное" : "различных",
                amounts.size(), nDiff(amounts), nDiff(amounts) == 1 ? "различная" : "различных",
                numbers.size(), nDiff(numbers), nDiff(numbers) == 1 ? "различное" : "различных",
                dates.size(), nDiff(dates), nDiff(dates) == 1 ? "различная" : "различных",

                sentences.size(), nDiff(sentences), nDiff(sentences) == 1 ? "различное" : "различных",
                minStr(inLocale, sentences), maxStr(inLocale, sentences),
                minLength(number, sentences), argMinLength(sentences),
                maxLength(number, sentences), argMaxLength(sentences),
                avgLength(number, sentences),

                words.size(), nDiff(words), nDiff(words) == 1 ? "различное" : "различных",
                minStr(inLocale, words), maxStr(inLocale, words),
                minLength(number, words), argMinLength(words),
                maxLength(number, words), argMaxLength(words),
                avgLength(number, words),

                amounts.size(), nDiff(amounts), nDiff(amounts) == 1 ? "различная" : "различных",
                min(currency, amounts), max(currency, amounts), avg(currency, amounts),

                numbers.size(), nDiff(numbers), nDiff(numbers) == 1 ? "различное" : "различных",
                min(number, numbers), max(number, numbers), avg(number, numbers),

                dates.size(), nDiff(dates), nDiff(dates) == 1 ? "различная" : "различных",
                min(date, dates), max(date, dates), avgDate(locale, dates)
        );
    }

    protected static String generateReport(final String fileName, final Locale inLocale, final boolean isRu,
                                           final List<String> sentences, final List<String> words,
                                           final List<Double> numbers, final List<Double> amounts,
                                           final List<LocalDate> dates) {
        final var newNumbers = numbers.stream().map(BigDecimal::valueOf).collect(Collectors.toUnmodifiableList());
        final var newAmounts = amounts.stream().map(BigDecimal::valueOf).collect(Collectors.toUnmodifiableList());
        final var newDates = dates.stream().map(d -> Date.from(d.atStartOfDay(ZoneId.systemDefault()).toInstant()))
                .collect(Collectors.toUnmodifiableList());
        if (isRu) {
            return generateReport_ru_RU(fileName, inLocale, sentences, words, newNumbers, newAmounts, newDates);
        } else {
            return generateReport_en_US(fileName, inLocale, sentences, words, newNumbers, newAmounts, newDates);
        }
    }

    @Test
    public void test02_simple() {
        testAppText("Hello, antizhiza...", "input.txt", Locale.US,
                List.of("Hello, antizhiza..."), List.of("Hello", "antizhiza"), List.of(), List.of(), List.of());
    }

    @Test
    public void test03_dates() {
        testAppText("This is the first sentence, created on 05/31/2021. I'd like to receive $100 for it, " +
                        "negative number is -123. " +
                        "The day after tomorrow will be 06/02/2021.", "input.txt", Locale.US,
                List.of(
                        "This is the first sentence, created on 05/31/2021.",
                        "I'd like to receive $100 for it, negative number is -123.",
                        "The day after tomorrow will be 06/02/2021."
                ),
                List.of("This", "is", "the", "first", "sentence", "created", "on",
                        "I'd", "like", "to", "receive", "for", "it", "negative", "number", "is", "The", "day", "after",
                        "tomorrow", "will", "be"),
                List.of(5., 31., 2021., 100., -123., 6., 2., 2021.),
                List.of(100.),
                List.of(LocalDate.of(2021, Month.MAY, 31),
                        LocalDate.of(2021, Month.JUNE, 2)));
    }

    @Test
    public void test04_french() {
        testAppText("Le 8 janvier 1990, l'ascenseur me cracha au dernier étage de l'immeuble Yumimoto. " +
                        "Par exemple, si le cadre avait calcule que Yumimoto lui devait 93,327 €, j'obtenais " +
                        "15,211 €, ou alors 172,045 €. Vint la nuit du 30 au 31.", "input.txt", Locale.FRANCE,
                List.of(
                        "Le 8 janvier 1990, l'ascenseur me cracha au dernier étage de l'immeuble Yumimoto.",
                        "Par exemple, si le cadre avait calcule que Yumimoto lui devait 93,327 €, j'obtenais " +
                                "15,211 €, ou alors 172,045 €.",
                        "Vint la nuit du 30 au 31."
                ),
                List.of("Le", "janvier", "l'ascenseur", "me", "cracha", "au", "dernier", "étage", "de",
                        "l'immeuble", "Yumimoto", "Par", "exemple", "si", "le", "cadre", "avait", "calcule", "que",
                        "Yumimoto", "lui", "devait", "j'obtenais", "ou", "alors",
                        "Vint", "la", "nuit", "du", "au"),
                List.of(8., 1990., 93.327, 15.211, 172.045, 30., 31.),
                List.of(93.327, 15.211, 172.045),
                List.of(LocalDate.of(1990, Month.JANUARY, 8)));
    }

    @Test
    public void test05_chinese() {
        testAppText("該文本是中文的特殊文本，出於說明目的，是辛勤工作的一部分。 這是第二句話。 這是第三句話。", "input.txt",
                Locale.CHINA,
                List.of(
                        "該文本是中文的特殊文本，出於說明目的，是辛勤工作的一部分。",
                        "這是第二句話。",
                        "這是第三句話。"
                ),
                List.of("該文本是中文的特殊文本", "出於說明目的", "是辛勤工作的一部分", "這是第二句話", "這是第三句話"),
                List.of(),
                List.of(),
                List.of());
    }

    @Test
    public void test06_arabic() {
        testAppText("لقد ولدت في 18 فبراير 2002. أتلقى راتبًا قدره ج.م.\u200F ١٢٣٫٠٠. آمل حقًا أن أنهي دورة \"Java " +
                        "Advanced\" بدرجة \"C\" أو علامة أعلى.", "input.txt",
                Locale.forLanguageTag("ar-EG"),
                List.of(
                        "لقد ولدت في 18 فبراير 2002.",
                        "أتلقى راتبًا قدره ج.م.\u200F ١٢٣٫٠٠.",
                        "آمل حقًا أن أنهي دورة \"Java Advanced\" بدرجة \"C\" أو علامة أعلى."
                ),
                List.of("لقد", "ولدت", "في", "فبراير", "أتلقى", "راتبًا", "قدره", "ج.م", "آمل", "حقًا",
                        "أن", "أنهي", "دورة", "Java", "Advanced", "بدرجة", "C", "أعلى", "علامة", "أو"),
                List.of(18., 2002., 123.),
                List.of(123.),
                List.of(LocalDate.of(2002, Month.FEBRUARY, 18)));
    }

    @Test
    public void test07_russian() {
        testAppText("Для подсчета хеш-суммы используйте 64-битную версию алгоритма PJW.\n" +
                        "Если при чтении файла возникают ошибки, укажите в качестве его хеш-суммы 0000000000000000.\n" +
                        "Кодировка входного и выходного файлов — UTF-8.\n" +
                        "Если родительская директория выходного файла не существует, то соответствующий путь надо " +
                        "создать.\n" +
                        "Размеры файлов могут превышать размер оперативной памяти.", "input.txt",
                RU_LOCALE,
                List.of(
                        "Для подсчета хеш-суммы используйте 64-битную версию алгоритма PJW.",
                        "Если при чтении файла возникают ошибки, укажите в качестве его хеш-суммы 0000000000000000.",
                        "Кодировка входного и выходного файлов — UTF-8.",
                        "Если родительская директория выходного файла не существует, то соответствующий путь надо создать.",
                        "Размеры файлов могут превышать размер оперативной памяти."
                ),
                List.of("Для", "подсчета", "хеш-суммы", "используйте", "битную", "версию", "алгоритма", "PJW",
                        "Если", "при", "чтении", "файла", "возникают", "ошибки", "укажите", "в", "качестве", "его",
                        "хеш-суммы", "Кодировка", "входного", "и", "выходного", "файлов", "UTF", "Если",
                        "родительская", "директория", "выходного", "файла", "не", "существует", "то", "соответствующий",
                        "путь", "надо", "создать", "Размеры", "файлов", "могут", "превышать", "размер", "оперативной",
                        "памяти"),
                List.of(64., 0., -8.),
                List.of(),
                List.of());
    }

    @Test
    public void test08_1984() {
        testApp(app -> {
            try {
                final String model_en_US = Files.readString(Path.of("1984_report_en_US.txt"));
                final String model_ru_RU = Files.readString(Path.of("1984_report_ru_RU.txt"));
                Assert.assertEquals(model_en_US, app.generateReport(RU_LOCALE, Locale.US, Path.of("1984.txt")));
                Assert.assertEquals(model_ru_RU, app.generateReport(RU_LOCALE, RU_LOCALE, Path.of("1984.txt")));
            } catch (final IOException e) {
                throw new AssertionError("This is literally 1984", e);
            }
        });
    }
}
