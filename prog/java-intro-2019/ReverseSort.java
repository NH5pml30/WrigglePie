// import java.util.Scanner;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;
import java.util.function.UnaryOperator;
import java.util.stream.IntStream;

class LineData {
    final int number;
    final long sum;

    public LineData( long sum, int number ) {
        this.sum = sum;
        this.number = number;
    }
}

public class ReverseSort {
    public static void main( String[] args ) throws FileNotFoundException {
        FastScanner scanner = new FastScanner(System.in);

        List<LineData> lineList = new ArrayList<LineData>();

        List<Integer> ints = new ArrayList<>();
        List<Integer> lineOffset = new ArrayList<>();
        lineOffset.add(0);

        for (int i = 0; ; i++) {
            int num = 0;
            long sum = 0;
            while (scanner.hasNextIntInLine()) {
                num++;
                int cur = scanner.nextInt();
                ints.add(cur);
                sum += cur;
            }

            if (num != 0) {
                Collections.sort(ints.subList(lineOffset.get(i), lineOffset.get(i) + num));
            }
            lineList.add(new LineData(sum, i));
            lineOffset.add(lineOffset.get(i) + num);
            if (scanner.hasNextLine()) {
                scanner.skipLine();
            } else {
                break;
            }
        }

        lineList.sort(( LineData left, LineData right ) -> {
            if (left.sum < right.sum || (left.sum == right.sum && left.number < right.number)) {
                return 1;
            }
            return -1;
        });

        for (LineData line : lineList) {
            int num = line.number;
            for (int j = lineOffset.get(num + 1) - 1; j >= lineOffset.get(num); j--) {
                System.out.print(ints.get(j));
                if (j != 0) {
                    System.out.print(" ");
                }
            }
            System.out.println();
        }
    }
}
