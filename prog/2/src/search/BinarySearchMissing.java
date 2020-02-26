package search;

import static search.util.BinarySearch.*;

public class BinarySearchMissing extends BinarySearchBase {
    // pre:
    // valid 32-bit signed integer number in args[0] -- x
    // (if any) valid 32-bit signed integer numbers in args[1].. sorted non-incr -- arr
    // post:
    // prints res:
    //   if x in arr:
    //     first occurrence of x in arr
    //   else:
    //     res -- (-(insertion point) - 1),
    //     insertion point:
    //       index of first element less than x or arr.length if none exists
    public static void main(String[] args) {
        if (!instance.read(args)) {
            return;
        }
        int res = iterativeSearch(instance.arr, y -> y <= instance.x);
        // arr[y < res] > x && arr[y >= res] <= x
        if (res == instance.arr.length || instance.arr[res] != instance.x) {
            //   res == arr.length => arr[for all y] < x
            //   arr[res] != x => arr[y >= res] < arr[res] < x && arr[y < res] < x
            // => no x
            res = -res - 1;
        }
        // else arr[res] == x, arr[y < res] > x
        System.out.println(res);
    }
}
