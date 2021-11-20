package info.kgeorgiy.ja.holyavin.statistics;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Locale;

public interface TextStatisticsReporter {
    String generateReport(Locale inLocale, Locale outLocale, Path inFile) throws IOException;
}
