package search;

import static search.util.BinarySearch.*;

public class BinarySearchSpan extends BinarySearchBase {
    // pre:
    // valid 32-bit signed integer number in args[0] -- x
    // (if any) valid 32-bit signed integer numbers in args[1].. sorted non-incr -- arr
    // post:
    // prints l, count:
    //   if x in arr:
    //     arr[l..l+count) == x and only [l..l+count)
    //   else:
    //     l -- insertion point, count = 0
    //     insertion point:
    //       index of first element less than x or arr.length if none exists
    public static void main(String[] args) {
        if (!instance.read(args)) {
            return;
        }

        int res = iterativeSearch(instance.arr, y -> y <= instance.x);
        // arr[y < res] > x && arr[y >= res] <= x
        int left = res, count;

        if (res == instance.arr.length || instance.arr[res] != instance.x) {
            //   res == arr.length => arr[for all y] < x
            //   arr[res] != x => arr[y >= res] < arr[res] < x && arr[y < res] < x
            // => no x
            count = 0;
        } else {
            res = iterativeSearch(instance.arr, y -> y < instance.x);
            // arr[y < res] >= x && arr[y >= res] < x
            count = res - left;
        }
        System.out.println(left + " " + count);
    }
}
