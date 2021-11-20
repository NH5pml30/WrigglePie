import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

class WordData {
    int occurNum, appearOrder;

    public WordData( int occurNum, int appearOrder ) {
        this.occurNum = occurNum;
        this.appearOrder = appearOrder;
    }

    static WordData defaultData( int appearOrder ) {
        return new WordData(1, appearOrder);
    }

    static WordData update( WordData left, WordData right ) {
        return new WordData(left.occurNum + right.occurNum,
                            Integer.min(left.appearOrder, right.appearOrder));
    }
}

public class WordStatCount {
    private static boolean isWord( char Ch ) {
        return Character.isAlphabetic(Ch) ||
                   Character.getType(Ch) == Character.DASH_PUNCTUATION ||
                   Ch == '\'';
    }

    private static void CountStats( String inputFileName, HashMap<String, WordData> statsRes )
        throws IOException
    {
        BufferedReader reader = new BufferedReader(new InputStreamReader(
            new FileInputStream(inputFileName), StandardCharsets.UTF_8));

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
                statsRes.merge(word.toString(), WordData.defaultData(statsRes.size()), WordData::update);
                word.setLength(0);
            }

            if (ch == -1) {
                break;
            }
        }
        reader.close();
    }

    public static void main( String[] args ) throws IOException {
        BufferedWriter writer = new BufferedWriter(
            new OutputStreamWriter(new FileOutputStream(args[1]), StandardCharsets.UTF_8));
        HashMap<String, WordData> stats = new HashMap<>();

        CountStats(args[0], stats);
        List<Map.Entry<String, WordData>> list = new ArrayList<>(stats.entrySet());
        list.sort(( left, right ) -> {
            WordData l = left.getValue(), r = right.getValue();

            if (l.occurNum < r.occurNum ||
                (l.occurNum == r.occurNum && l.appearOrder < r.appearOrder)) {
                return -1;
            }
            return 1;
        });

        for (Map.Entry<String, WordData> entry : list) {
            writer.write(entry.getKey());
            writer.write(' ');
            writer.write(Integer.toString(entry.getValue().occurNum));
            writer.write('\n');
        }

        writer.close();
    }
}
