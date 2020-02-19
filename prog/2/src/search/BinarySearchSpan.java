package search;

import static search.util.BinarySearch.*;

public class BinarySearchSpan extends BinarySearchBase {
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
        int left = res, count;
        // arr is sorted non-increasing &&
        // (res == arr.length || arr[y >= res] <= x) && arr[y < res] > x &&
        // left == res

        if (res == instance.arr.length || instance.arr[res] != instance.x) {
            // doesn't exist y: arr[y] == x
            count = 0;
        } else {
            // arr is sorted non-increasing &&
            // arr[y >= left] <= x && arr[y < left] > x && arr[left] == x
            res = iterativeSearch(instance.arr, y -> y < instance.x);
            // arr is sorted non-increasing &&
            // arr[y >= left] <= x && arr[y < left] > x && arr[left] == x &&
            // (exists y: arr[y] == x)
            // arr[y >= res] < x && arr[y < res] >= x && arr[res] == x
            // =>
            // arr[left .. res-1] == x
            count = res - left;
        }
        // exists y: arr[y] == x ?
        //   arr[y < left] > x, arr[left..left+count-1] == x, arr[y > left+count] < x :
        //   place for x is at left, count == 0
        System.out.println(left + " " + count);
    }
}
