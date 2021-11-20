import java.util.Scanner;
import java.util.ArrayList;
import java.util.function.UnaryOperator;

public class Reverse {
    public static void main(String[] args) {
        Scanner stringScanner = new Scanner(System.in);

        UnaryOperator<Integer> fun = ( Integer x ) -> x * 5;
        IntVector
            ints = new IntVector(fun),
            intsPerLine = new IntVector(fun);

        for (int i = 0; stringScanner.hasNextLine(); i++) {
            Scanner intScanner = new Scanner(stringScanner.nextLine());

            int num = 0;
            while (intScanner.hasNextInt()) {
                num++;
                ints.add(intScanner.nextInt());
            }

            intsPerLine.add(num);
        }

        int intIndex = ints.size();
        for (int i = intsPerLine.size() - 1; i >= 0; i--) {
            for (int j = intsPerLine.get(i) - 1; j >= 0; j--) {
                System.out.print(ints.get(--intIndex));
                if (j != 0) {
                    System.out.print(" ");
                }
            }
            System.out.println();
        }
    }
}
