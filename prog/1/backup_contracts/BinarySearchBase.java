package search;

public class BinarySearchBase {
    protected int x;
    protected int[] arr;
    protected static BinarySearchBase instance = new BinarySearchBase();

    // pre: numbers in args[1].. (if any) sorted non-incr
    // post:
    // if r == true, then
    //   args.length >= 1 && x is valid &&
    //   arr.length == args.length - 1 &&
    //   arr[i] is valid (sorted non-increasing) and read (i in arr)
    protected boolean read(String[] args) {
        if (args.length < 1) {
            System.out.println("Not enough arguments (needs at least 1)");
            return false;
        }

        // numbers in args[1].. (if any) sorted non-incr &&
        // args.length >= 1
        arr = new int[args.length - 1];
        // numbers in args[1].. (if any) sorted non-incr &&
        // args.length >= 1 && arr.length == args.length - 1
        int j = 0;
        try {
            x = Integer.parseInt(args[0]);
            // numbers in args[1].. sorted non-incr &&
            // args.length >= 1 && arr.length == args.length - 1 && x is valid
            int i = 0;
            for (j = 1; j < args.length; j++, i++) {
                // numbers in args[1].. sorted non-incr &&
                // args.length >= 1 && x is valid &&
                // arr.length == args.length - 1 &&
                // j < args.length && i < arr.length &&
                // arr[k] is valid (sorted non-increasing) and read (k < i)
                arr[i] = Integer.parseInt(args[j]);
            }
            // args.length >= 1 && x is valid &&
            // arr.length == args.length - 1 &&
            // arr[i] is valid (sorted non-increasing) and read (i in arr)
        } catch (NumberFormatException e) {
            System.out.println("Invalid number format at " + j + ": " + e.getMessage());
            return false;
        }
        // => post
        return true;
    }
}
