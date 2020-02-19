package search;

import static search.util.BinarySearch.*;

public class BinarySearchMissing extends BinarySearchBase {
    public static void main(String[] args) {
        // pre: numbers in args[1].. sorted non-incr

        if (!instance.read(args)) {
            return;
        }
        // args.length >= 1 && x is valid &&
        // arr.length == args.length - 1 &&
        // arr[i] is valid (sorted non-increasing) and read (i in arr)
        int res = iterativeSearch(instance.arr, y -> y <= instance.x);
        // arr is sorted non-increasing &&
        // (res == arr.length || arr[y >= res] <= x) && arr[y < res] > x
        if (res == instance.arr.length || instance.arr[res] != instance.x) {
            // arr is sorted non-increasing &&
            // (res == arr.length || arr[y >= res] <= x) && arr[y < res] > x &&
            // (res == arr.length || arr[res] != x)
            // =>
            // res == arr.length && arr[y < res] != x ||
            // arr[res] != x && arr[y > res] <= arr[res] < x && arr[y < res] > x
            // =>
            // doesn't exist y: arr[y] == x &&
            // arr[y < res] > x && arr[y >= res] < x
            res = -res - 1;
        }
        // x in arr ?
        //   arr[y >= res] <= x && arr[y < res] > x :
        //   place for x is at -(res + 1)
        System.out.println(res);
    }
}
