package search.util;

public class BinarySearchFunc {
    private BinarySearchInterface i;

    public BinarySearchFunc(BinarySearchInterface i) {
        this.i = i;
    }

    // pre: at in [-1 : arr.length]
    // post:
    // (if any) apply(y < at) <= r && (if any) apply(y > at) >= r &&
    // apply(at == -1) == false && apply(at == arr.length) == true
    boolean apply(int[] arr, int at) {
        if (at == -1) {
            return false;
        }
        // at >= 0
        if (at == arr.length) {
            return true;
        }
        // at >= 0 && at < arr.length
        return i.apply(arr[at]);
        // i.apply is monotonically increasing on [0 : arr.length) &&
        // apply(at == -1) == false && apply(at == arr.length) == true
        // <=>
        // apply is monotonically increasing on [-1 : arr.length] &&
        // apply(at == -1) == false && apply(at == arr.length) == true
        // => post
    }
}
