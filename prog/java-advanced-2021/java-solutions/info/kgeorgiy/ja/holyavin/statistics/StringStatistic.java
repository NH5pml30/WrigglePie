package info.kgeorgiy.ja.holyavin.statistics;

import java.text.MessageFormat;
import java.util.Locale;
import java.util.ResourceBundle;

public interface StringStatistic extends Statistic<String, Double> {
    Integer getMinLength();
    Integer getMaxLength();
    String getArgMinLength();
    String getArgMaxLength();

    default Double getStatOrDefault(Double val) {
        return val == null ? -1 : val;
    }

    default Integer getStatOrDefault(Integer val) {
        return val == null ? -1 : val;
    }

    default String getStatOrDefault(String val) {
        return val == null ? "<None>" : "\"" + val + "\"";
    }

    @Override
    default String formatReport(Locale locale, ResourceBundle bundle) {
        return new MessageFormat(bundle.getString(getName() + "Stats"), locale).format(new Object[] {
                System.lineSeparator(),
                getOccurrences(), getDiffOccurrences(),
                getStatOrDefault(getMinValue()),
                getStatOrDefault(getMaxValue()),
                getStatOrDefault(getMinLength()), getStatOrDefault(getArgMinLength()),
                getStatOrDefault(getMaxLength()), getStatOrDefault(getArgMaxLength()),
                getStatOrDefault(getAverage())
        });
    }
}
