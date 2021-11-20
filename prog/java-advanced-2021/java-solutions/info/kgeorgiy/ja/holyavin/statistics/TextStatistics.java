package info.kgeorgiy.ja.holyavin.statistics;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.text.*;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class TextStatistics implements TextStatisticsReporter {
    private static <T, V> Map<String, T> mapMap(final Map<String, V> map, final BiFunction<String, ? super V, ? extends T> mapper) {
        return map.entrySet().stream().map(entry ->
                Map.entry(
                        entry.getKey(),
                        mapper.apply(entry.getKey(), entry.getValue())
                )).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }

    private static <T extends Reportable, V> Stream<Reportable> add(final Stream<Reportable> stream,
                                                                    final Map<String, V> map,
                                                                    final BiFunction<String, ? super V, ? extends T> mapper) {
        return Stream.concat(stream, mapMap(map, mapper).values().stream());
    }

    private static String generateReport(final String fileName, final Locale outLocale, final List<Reportable> reportables) {
        final ResourceBundle bundle = ResourceBundle.getBundle("info.kgeorgiy.ja.holyavin.statistics.StatResourceBundle", outLocale);

        final StringBuilder builder = new StringBuilder(MessageFormat.format(bundle.getString("analyzedFile"), fileName));
        builder.append(System.lineSeparator()).append(bundle.getString("summaryStats")).append(System.lineSeparator());
        for (final var reportable : reportables) {
            builder.append(reportable.formatSummaryReport(outLocale, bundle)).append(System.lineSeparator());
        }
        for (final var reportable : reportables) {
            builder.append(reportable.formatReport(outLocale, bundle)).append(System.lineSeparator());
        }
        return builder.toString();
    }

    @Override
    public String generateReport(final Locale inLocale, final Locale outLocale, final Path inFile) throws IOException {
        final String text = Files.readString(inFile);

        final Map<String, BreakIterator> iterators = Map.of(
                "words", BreakIterator.getWordInstance(inLocale),
                "sentences", BreakIterator.getSentenceInstance(inLocale)
        );
        final Map<String, Statistic.FormatFactory> numberFormats = Map.of(
                "numbers", DecimalFormat::getInstance,
                "amounts", l -> {
                    final var res = DecimalFormat.getCurrencyInstance(l);
                    res.setCurrency(Currency.getInstance(inLocale));
                    return res;
                }
        );
        final Calendar calendar = Calendar.getInstance(inLocale);
        final List<Integer> formats = List.of(DateFormat.MEDIUM, DateFormat.FULL, DateFormat.LONG, DateFormat.SHORT);
        final List<Statistic.FormatFactory> dateFormats = formats.stream().map(
                f -> (Statistic.FormatFactory) (Locale l) -> DateFormat.getDateInstance(f, l)
        ).collect(Collectors.toUnmodifiableList());
        final Statistic.FormatFactory dateOrFormat = locale -> new Format() {
            private final Format outFormat = dateFormats.get(0).create(locale);
            private final List<Format> inFormats = dateFormats.stream().map(f -> f.create(locale))
                    .collect(Collectors.toUnmodifiableList());

            @Override
            public StringBuffer format(final Object obj, final StringBuffer toAppendTo, final FieldPosition pos) {
                return outFormat.format(obj, toAppendTo, pos);
            }

            @Override
            public Object parseObject(final String source, final ParsePosition pos) {
                Object val;
                for (final var format : inFormats) {
                    if ((val = format.parseObject(source, pos)) != null) {
                        return val;
                    }
                }
                return null;
            }
        };

        final StatCollector collector = new StatCollector();
        Stream<Reportable> stats = add(Stream.of(), iterators, (n, i) -> collector.collect(inLocale, text, n, i,
                n.equals("words")
                        ? s -> Character.isLetter(s.charAt(0)) ? s : null
                        : String::trim));
        stats = add(stats, numberFormats,
                (n, f) -> {
                    final Format ff = f.create(inLocale);
                    if (ff instanceof DecimalFormat) {
                        final DecimalFormat df = (DecimalFormat) ff;
                        df.setParseBigDecimal(true);
                        return collector.collect(text, n, Number::doubleValue, Function.identity(), f, f, df::parse);
                    } else {
                        return collector.collect(text, n, Number::doubleValue, Function.identity(), f, f, (s, p) -> {
                            final Object res = ff.parseObject(s, p);
                            return res instanceof Long
                                    ? BigDecimal.valueOf((Long) res)
                                    : res instanceof Double
                                    ? BigDecimal.valueOf((Double) res)
                                    : null;
                        });
                    }
                });
        stats = Stream.concat(stats, Stream.of(collector.<Date, Date>collect(
                inLocale,
                text,
                "dates",
                date -> (double) ChronoUnit.DAYS.between(calendar.toInstant(), date.toInstant()),
                days -> Date.from(calendar.toInstant().plus(days.longValue(), ChronoUnit.DAYS)),
                dateOrFormat, dateOrFormat
        )));

        return generateReport(inFile.getFileName().toString(), outLocale, stats.collect(Collectors.toUnmodifiableList()));
    }

    private static void printUsage(final ResourceBundle bundle) {
        System.out.printf(
                "%s TextStatistics %s %s %s %s%n",
                bundle.getString("usage"),
                bundle.getString("inputLocale"),
                bundle.getString("outputLocale"),
                bundle.getString("inputFile"),
                bundle.getString("outputFile")
        );
    }

    private static final String USAGE_BUNDLE_NAME = "info.kgeorgiy.ja.holyavin.statistics.UsageResourceBundle";

    public static void main(final String[] args) {
        ResourceBundle bundle = ResourceBundle.getBundle(USAGE_BUNDLE_NAME);

        if (args == null || args.length < 4 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            printUsage(bundle);
            return;
        }
        try {
            final Locale in = Locale.forLanguageTag(args[0]);
            final Locale out = Locale.forLanguageTag(args[1]);
            bundle = ResourceBundle.getBundle(USAGE_BUNDLE_NAME, out);
            final String report = new TextStatistics().generateReport(
                    in, out,
                    Path.of(args[2])
            );
            try {
                Files.writeString(Path.of(args[3]), report);
            } catch (final IOException e) {
                // :NOTE: Дубли
                System.out.print(MessageFormat.format(bundle.getString("writeError"), e.toString()));
            }
        } catch (final IOException e) {
            System.out.print(MessageFormat.format(bundle.getString("readError"), e.toString()));
        } catch (final InvalidPathException e) {
            System.out.print(MessageFormat.format(bundle.getString("invalidPath"), e.toString()));
        }
    }
}
