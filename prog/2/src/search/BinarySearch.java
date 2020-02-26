package search;

import static search.util.BinarySearch.*;

public class BinarySearch extends BinarySearchBase {
    // pre:
    // valid 32-bit signed integer number in args[0] -- x
    // (if any) valid 32-bit signed integer numbers in args[1].. sorted non-incr -- arr
    // post:
    // prints res:
    //   if exists element <= x:
    //     arr[res] <= x, res is minimum
    //   else:
    //     res = arr.length
    public static void main(String[] args) {
        if (!instance.read(args)) {
            return;
        }
        int res = iterativeSearch(instance.arr, y -> y <= instance.x);
        // arr[y < res] > x && arr[y >= res] <= x
        System.out.println(res);
    }
}
