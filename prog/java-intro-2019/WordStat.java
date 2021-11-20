import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Map;

class WordStat {
    private static boolean isWord( char Ch ) {
        return Character.isAlphabetic(Ch) ||
                   Character.getType(Ch) == Character.DASH_PUNCTUATION ||
                   Ch == '\'';
    }

    static <T extends Map<String, Integer>> void CountStats( String InputFileName, T StatsRes )
        throws IOException
    {
        BufferedReader reader = new BufferedReader(new InputStreamReader(
            new FileInputStream(InputFileName), StandardCharsets.UTF_8));

        StringBuilder word = new StringBuilder();
        while (true) {
            int ch;

            while ((ch = reader.read()) != -1 && !isWord((char)ch)) {
                ;
            }
            if (ch == -1) {
                break;
            }

            do {
                word.append(Character.toLowerCase((char)ch));
            } while ((ch = reader.read()) != -1 && isWord((char)ch));

            if (word.length() != 0) {
                StatsRes.merge(word.toString(), 1, Integer::sum);
                word.setLength(0);
            }

            if (ch == -1) {
                break;
            }
        }
        reader.close();
    }
}
