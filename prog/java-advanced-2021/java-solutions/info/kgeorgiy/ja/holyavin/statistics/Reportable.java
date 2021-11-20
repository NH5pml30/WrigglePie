package info.kgeorgiy.ja.holyavin.statistics;

import java.util.Locale;
import java.util.ResourceBundle;

public interface Reportable {
    String getName();
    String formatSummaryReport(Locale locale, ResourceBundle bundle);
    String formatReport(Locale locale, ResourceBundle bundle);
}
