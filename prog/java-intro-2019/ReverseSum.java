import java.util.Scanner;
import java.util.ArrayList;
import java.util.function.UnaryOperator;

public class ReverseSum {
	public static void main(String[] args) {
		Scanner stringScanner = new Scanner(System.in);

		UnaryOperator<Integer> fun = ( Integer x ) -> x * 5;
		IntVector
			sumRow = new IntVector(fun),
			sumCol = new IntVector(fun),
			intsPerLine = new IntVector(fun),
			ints = new IntVector(fun);
		
		for (int i = 0; stringScanner.hasNextLine(); i++) {
			Scanner intScanner = new Scanner(stringScanner.nextLine());

			sumRow.add(0);
			int num = 0;
			while (intScanner.hasNextInt()) {
				if (num == sumCol.size()) {
					sumCol.add(0);
				}

				int curNumber = intScanner.nextInt();
				sumCol.set(num, sumCol.get(num) + curNumber);
				num++;
				sumRow.set(i, sumRow.get(i) + curNumber);
				ints.add(curNumber);
			}
			
			intsPerLine.add(num);
		}

		int intIndex = 0;
        for (int i = 0; i < sumRow.size(); i++) {
			int num = intsPerLine.get(i);
            for (int j = 0; j < num; j++) {
                System.out.print(sumRow.get(i) + sumCol.get(j) - ints.get(intIndex++));
                if (j != num - 1) {
                    System.out.print(" ");
                }
            }
			System.out.println();
        }
    }
}
