package search;

public class BinarySearchBase {
    protected int x;
    protected int[] arr;
    protected static BinarySearchBase instance = new BinarySearchBase();

    // pre:
    // valid 32-bit signed integer number in args[0] -- x
    // (if any) valid 32-bit signed integer numbers in args[1].. sorted non-incr -- arr
    // post:
    // if r == true, then
    //   x is read from args[0] &&
    //   arr.length == args.length - 1 >= 0 &&
    //   arr[i] is read (from args[i+1]) &&
    //   (if exists) arr sorted non-increasing
    protected boolean read(String[] args) {
        if (args.length < 1) {
            System.out.println("Not enough arguments (needs at least 1)");
            return false;
        }

        arr = new int[args.length - 1];
        int j = 0;
        try {
            x = Integer.parseInt(args[0]);
            int i = 0;
            for (j = 1; j < args.length; j++, i++) {
                arr[i] = Integer.parseInt(args[j]);
            }
        } catch (NumberFormatException e) {
            System.out.println("Invalid number format at " + j + ": " + e.getMessage());
            return false;
        }
        return true;
    }
}
