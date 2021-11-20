package info.kgeorgiy.ja.holyavin.statistics;

import java.text.BreakIterator;
import java.text.Collator;
import java.text.Format;
import java.text.ParsePosition;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;

class ConcreteStatistic<T, AvgT> implements Statistic<T, AvgT> {
    private final String name;
    private final int occurrences, diffOccurrences;
    private final T minValue, maxValue;
    private final AvgT average;
    private final FormatFactory formatT;
    private final FormatFactory formatAvgT;

    @Override
    public String getName() {
        return name;
    }

    @Override
    public int getOccurrences() {
        return occurrences;
    }

    @Override
    public int getDiffOccurrences() {
        return diffOccurrences;
    }

    @Override
    public T getMinValue() {
        return minValue;
    }

    @Override
    public T getMaxValue() {
        return maxValue;
    }

    @Override
    public AvgT getAverage() {
        return average;
    }

    @Override
    public FormatFactory getFormatT() {
        return formatT;
    }

    @Override
    public FormatFactory getFormatAvgT() {
        return formatAvgT;
    }

    ConcreteStatistic(String name, int occurrences, int diffOccurrences, T minValue, T maxValue, AvgT average,
                      FormatFactory formatT, FormatFactory formatAvgT) {
        this.name = name;
        this.occurrences = occurrences;
        this.diffOccurrences = diffOccurrences;
        this.minValue = minValue;
        this.maxValue = maxValue;
        this.average = average;
        this.formatT = formatT;
        this.formatAvgT = formatAvgT;
    }
}

class ConcreteStringStatistic extends ConcreteStatistic<String, Double> implements StringStatistic {
    private final String argMinLength, argMaxLength;

    @Override
    public Integer getMinLength() {
        return argMinLength == null ? null : argMinLength.length();
    }

    @Override
    public Integer getMaxLength() {
        return argMaxLength == null ? null : argMaxLength.length();
    }

    @Override
    public String getArgMinLength() {
        return argMinLength;
    }

    @Override
    public String getArgMaxLength() {
        return argMaxLength;
    }

    public ConcreteStringStatistic(ConcreteStatistic<String, Double> base, String argMinLength, String argMaxLength) {
        super(base.getName(), base.getOccurrences(), base.getDiffOccurrences(),
                base.getMinValue(), base.getMaxValue(), base.getAverage(), null, null);
        this.argMinLength = argMinLength;
        this.argMaxLength = argMaxLength;
    }
}

public class StatCollector {
    private static class Util {
        @SuppressWarnings("unchecked") // want to throw on cast fail
        private static <C> int elementsCompareNatural(C lhs, C rhs) {
            return ((Comparable<C>)lhs).compareTo(rhs); // pre: lhs, rhs not null
        }

        private static <C> int elementsCompare(C lhs, C rhs, Comparator<? super C> comparator) {
            return comparator == null ? elementsCompareNatural(lhs, rhs) : comparator.compare(lhs, rhs);
        }

        static <C, T> C relaxMin(C min, C val, Function<? super C, ? extends T> mapper,
                                 Comparator<? super T> comparator) {
            return min == null ? val :
                    elementsCompare(mapper.apply(val), mapper.apply(min), comparator) < 0 ? val : min;
        }

        static <C, T> C relaxMax(C max, C val, Function<? super C, ? extends T> mapper,
                                 Comparator<? super T> comparator) {
            return max == null ? val :
                    elementsCompare(mapper.apply(val), mapper.apply(max), comparator) > 0 ? val : max;
        }
    }

    private static class MinMaxStat<C, T> {
        private C min = null, max = null;
        private final Comparator<? super T> comparator;
        private final Function<? super C, ? extends T> mapper;

        MinMaxStat(Comparator<? super T> comparator, Function<? super C, ? extends T> mapper) {
            this.comparator = comparator;
            this.mapper = mapper;
        }

        void relax(C val) {
            min = Util.relaxMin(min, val, mapper, comparator);
            max = Util.relaxMax(max, val, mapper, comparator);
        }

        C getMin() {
            return min;
        }

        C getMax() {
            return max;
        }
    }

    private static class Collector<T, AvgT> {
        final Function<T, Double> toAveragable;
        final Function<Double, AvgT> fromAveragable;

        final Set<T> set = new HashSet<>();
        int counter = 0;
        final MinMaxStat<T, T> minmax;
        double averagableSum = 0;

        Collector(Comparator<? super T> comparator,
                  Function<T, Double> toAveragable,
                  Function<Double, AvgT> fromAveragable) {
            this.toAveragable = toAveragable;
            this.fromAveragable = fromAveragable;
            this.minmax = new MinMaxStat<>(comparator, Function.identity());
        }

        void add(T val) {
            counter++;
            set.add(val);
            minmax.relax(val);
            averagableSum += toAveragable.apply(val);
        }

        ConcreteStatistic<T, AvgT> exportData(String name,
                                              Statistic.FormatFactory formatT,
                                              Statistic.FormatFactory formatAvgT) {
            return new ConcreteStatistic<>(name, counter, set.size(), minmax.getMin(), minmax.getMax(),
                    counter == 0 ? null : fromAveragable.apply(averagableSum / counter),
                    formatT, formatAvgT);
        }
    }

    private static class StringCollector extends Collector<String, Double> {
        final MinMaxStat<String, Integer> argMinMaxLength;

        StringCollector(Locale locale) {
            super(Collator.getInstance(locale), str -> (double) str.length(), Function.identity());
            argMinMaxLength = new MinMaxStat<>(null, String::length);
        }

        void add(String str) {
            super.add(str);
            argMinMaxLength.relax(str);
        }

        ConcreteStringStatistic exportData(String name) {
            return new ConcreteStringStatistic(super.exportData(name, null, null),
                    argMinMaxLength.getMin(), argMinMaxLength.getMax());
        }
    }

    public StringStatistic collect(Locale locale, String text, String name, BreakIterator boundary,
                                   Function<String, String> compute) {
        boundary.setText(text);
        StringCollector collector = new StringCollector(locale);
        int start = boundary.first();
        for (int end = boundary.next(); end != BreakIterator.DONE; start = end, end = boundary.next()) {
            String str = text.substring(start, end);
            if ((str = compute.apply(str)) != null) {
                collector.add(str);
            }
        }
        return collector.exportData(name);
    }

    @SuppressWarnings("unchecked")
    public <T, AvgT> Statistic<T, AvgT> collect(String text, String name,
                                                Comparator<? super T> comparator,
                                                Function<T, Double> toAveragable, Function<Double, AvgT> fromAveragable,
                                                Statistic.FormatFactory formatT, Statistic.FormatFactory formatAvgT,
                                                BiFunction<String, ParsePosition, Object> formatMethod) {
        Collector<T, AvgT> collector = new Collector<>(comparator, toAveragable, fromAveragable);
        ParsePosition position = new ParsePosition(0);
        while (position.getIndex() < text.length()) {
            T val = (T) formatMethod.apply(text, position);
            if (val == null) {
                position.setIndex(position.getIndex() + 1);
            } else {
                collector.add(val);
            }
        }
        return collector.exportData(name, formatT, formatAvgT);
    }

    public <T, AvgT> Statistic<T, AvgT> collect(String text, String name,
                                                Function<T, Double> toAveragable, Function<Double, AvgT> fromAveragable,
                                                Statistic.FormatFactory formatT, Statistic.FormatFactory formatAvgT,
                                                BiFunction<String, ParsePosition, Object> formatMethod) {
        return collect(text, name, null, toAveragable, fromAveragable,
                formatT, formatAvgT, formatMethod);
    }

    public <T, AvgT> Statistic<T, AvgT> collect(Locale locale, String text, String name,
                                                Comparator<? super T> comparator,
                                                Function<T, Double> toAveragable, Function<Double, AvgT> fromAveragable,
                                                Statistic.FormatFactory formatT, Statistic.FormatFactory formatAvgT) {
        return collect(text, name, comparator, toAveragable, fromAveragable,
                formatT, formatAvgT, formatT.create(locale)::parseObject);
    }

    public <T, AvgT> Statistic<T, AvgT> collect(Locale locale, String text, String name,
                                                Function<T, Double> toAveragable, Function<Double, AvgT> fromAveragable,
                                                Statistic.FormatFactory formatT, Statistic.FormatFactory formatAvgT) {
        return collect(locale, text, name, null, toAveragable, fromAveragable, formatT, formatAvgT);
    }
}
