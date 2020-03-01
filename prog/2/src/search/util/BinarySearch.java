package search.util;

public final class BinarySearch {
    private BinarySearch() {}

    // pre: none
    // post:
    // (if any) f(y < r) == false && (if any) f(y >= r) == true &&
    // arr.length >= r >= 0
    public static int iterativeSearch(int[] arr, BinarySearchInterface f) {
        // f is monotonically increasing on [0 : arr.length)
        BinarySearchFunc func = new BinarySearchFunc(f);

        // func is monotonically increasing on [-1 : arr.length] &&
        // func(-1) == false, func(arr.length) == true
        int L = -1, R = arr.length, M;
        // global_inv(L,R):
        // func is monotonically increasing on [-1 : arr.length] &&
        // arr.length >= R > L >= -1 &&
        // func(L) == false && func(R) == true
        while (L < R - 1) {
            // inv: global_inv && R - L > 1
            M = L / 2 + R / 2 + (L % 2 + R % 2) / 2;
            // M = (L + R) / 2
            if (func.apply(arr, M)) {
                R = M;
            } else {
                L = M;
            }
            // global_inv(L',R') &&
            // |R' - L'| = |R - L| / 2
        }
        // global_inv(L,R) && R == L - 1
        // =>
        // func is monotonically increasing on [-1 : arr.length] &&
        // arr.length >= R >= 0 &&
        // func(R - 1) == false && func(R) == true
        // => post
        return R;
    }

    // pre: func(L) == false && func(R) == true && arr.length >= R > L >= -1
    // post:
    // func(y < r) == false && func(y >= r) == true &&
    // arr.length >= r >= 0
    private static int inRecursive(int[] arr, BinarySearchFunc func, int L, int R) {
        // inv(L,R):
        // func is monotonically increasing on [-1 : arr.length] &&
        // func(L) == false && func(R) == true && arr.length >= R > L >= -1
        if (L < R - 1) {
            // R - L > 1
            int M = L / 2 + R / 2 + (L % 2 + R % 2) / 2;
            // R - L > 1 && M = (L + R) / 2
            if (func.apply(arr, M)) {
                // inv(L,M) && R - L > 1 &&
                // |L - M| = |R - L| / 2
                return inRecursive(arr, func, L, M);
                // post: func(y < r) == false && func(y >= r) == true &&
                // arr.length >= r >= 0
            } else {
                // inv(M,R) && R - L > 1 &&
                // |R - M| = |R - L| / 2
                return inRecursive(arr, func, M, R);
                // => post
            }
        } else {
            // (inv &&) R - L == 1
            // => post
            return R;
        }
    }

    // pre:
    // post:
    // (if any) f(y < r) == false && (if any) f(y >= r) == true &&
    // arr.length >= r >= 0
    public static int recursiveSearch(int[] arr, BinarySearchInterface f) {
        // f is monotonically increasing on [0 : arr.length)
        BinarySearchFunc func = new BinarySearchFunc(f);
        // func is monotonically increasing on [-1 : arr.length] &&
        //
        // func(L = -1) == false && func(R = arr.length) == true
        // <=>
        // func(L) == false && func(R) == true && arr.length >= R > L >= -1
        return inRecursive(arr, func, -1, arr.length);
        // => post
    }
}
