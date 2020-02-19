package search.util;

public class BinarySearchFunc {
    private BinarySearchInterface i;

    public BinarySearchFunc(BinarySearchInterface i) {
        this.i = i;
    }

    boolean apply(int[] arr, int at) {
        // pre: at in [-1 : arr.length]

        if (at == -1) {
            return false;
        }
        // at >= 0
        if (at == arr.length) {
            return true;
        }
        // at >= 0 && at < arr.length
        return i.apply(arr[at]);
        // post:
        // i.apply is monotonically increasing on [0 : arr.length) &&
        // apply(at == -1) == false && apply(at == arr.length) == true
        // <=>
        // apply is monotonically increasing on [-1 : arr.length] &&
        // apply(at == -1) == false && apply(at == arr.length) == true
        // <=>
        // apply(y < r) <= r && apply(y > r) >= r &&
        // apply(at == -1) == false && apply(at == arr.length) == true
    }
}
