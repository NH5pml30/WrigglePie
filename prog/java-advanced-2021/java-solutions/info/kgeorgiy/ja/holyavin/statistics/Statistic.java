package info.kgeorgiy.ja.holyavin.statistics;

import java.text.ChoiceFormat;
import java.text.Format;
import java.text.MessageFormat;
import java.util.Locale;
import java.util.ResourceBundle;

public interface Statistic<T, AvgT> extends Reportable {
    interface FormatFactory {
        Format create(Locale locale);
    }

    int getOccurrences();
    int getDiffOccurrences();
    T getMinValue();
    T getMaxValue();
    AvgT getAverage();

    FormatFactory getFormatT();
    FormatFactory getFormatAvgT();

    @Override
    default String formatSummaryReport(Locale locale, ResourceBundle bundle) {
        return new MessageFormat(bundle.getString(getName() + "N"), locale).format(new Object[] {
                getOccurrences(),
                getDiffOccurrences()
        });
    }

    default String getStatOrDefault(Object val, Format format) {
        return val == null ? "<None>" : format.format(val);
    }

    @Override
    default String formatReport(Locale locale, ResourceBundle bundle) {
        Format tFormat = getFormatT().create(locale), avgFormat = getFormatAvgT().create(locale);
        return new MessageFormat(bundle.getString(getName() + "Stats"), locale).format(new Object[] {
                System.lineSeparator(),
                getOccurrences(),
                getDiffOccurrences(),
                getStatOrDefault(getMinValue(), tFormat),
                getStatOrDefault(getMaxValue(), tFormat),
                getStatOrDefault(getAverage(), avgFormat)
        });
    }
}
