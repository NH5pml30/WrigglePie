package search;

import static search.util.BinarySearch.*;

public class BinarySearch extends BinarySearchBase {
    // pre: numbers in args[1]... sorted non-incr
    public static void main(String[] args) {
        if (!instance.read(args)) {
            return;
        }
        // args.length >= 1 && x is valid &&
        // arr.length == args.length - 1 &&
        // arr[i] is valid (sorted non-increasing) and read (i in arr)
        int res = iterativeSearch(instance.arr, y -> y <= instance.x);
        // (res == arr.length || arr[y >= res] <= x) && arr[y < res] > x
        System.out.println(res);
    }
}
