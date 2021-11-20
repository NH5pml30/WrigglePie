import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Map;

public class WordStatInput {
    public static void main( String[] args ) throws IOException {
        BufferedWriter writer = new BufferedWriter(
            new OutputStreamWriter(new FileOutputStream(args[1]), StandardCharsets.UTF_8));
        LinkedHashMap<String, Integer> stats = new LinkedHashMap<>();

        WordStat.CountStats(args[0], stats);
        for (Map.Entry<String, Integer> entry : stats.entrySet()) {
            writer.write(entry.getKey());
            writer.write(' ');
            writer.write(entry.getValue().toString());
            writer.write('\n');
        }

        writer.close();
    }
}
