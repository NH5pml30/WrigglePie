package search.util;

public final class BinarySearch {
    private BinarySearch() {}

    public static int iterativeSearch(int[] arr, BinarySearchInterface f) {
        // pre: none

        // f is monotonically increasing on [0 : arr.length)
        BinarySearchFunc func = new BinarySearchFunc(f);

        // func is monotonically increasing on [-1 : arr.length] &&
        // func(-1) == false, func(arr.length) == true
        int L = -1, R = arr.length, M;
        // global_inv(L,R):
        // func is monotonically increasing on [-1 : arr.length] &&
        // func(L) == false && func(R) == true && R >= 0
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
        // func(R - 1) == false && func(R) == true && R >= 0
        // =>
        // post:
        // f(y < R) == false && (if any) f(y >= R) == true && R >= 0
        // (&& R <= arr.length)
        return R;
    }

    private static int inRecursive(int[] arr, BinarySearchFunc f, int L, int R) {
        // pre: func(L) == false && func(R) == true && R >= 0

        // inv(L,R):
        // func is monotonically increasing on [-1 : arr.length] &&
        // func(L) == false && func(R) == true && R >= 0 && L < arr.length
        if (L < R - 1) {
            // R - L > 1
            int M = L / 2 + R / 2 + (L % 2 + R % 2) / 2;
            // R - L > 1 && M = (L + R) / 2
            if (f.apply(arr, M)) {
                // inv(L,M) && R - L > 1 &&
                // |L - M| = |R - L| / 2
                return inRecursive(arr, f, L, M);
                // post: func(y < r) == false && func(y >= r) == true && R >= 0
                // (&& R <= arr.length)
            } else {
                // inv(M,R) && R - L > 1 &&
                // |R - M| = |R - L| / 2
                return inRecursive(arr, f, M, R);
                // func(y < r) == false && func(y >= r) == true && R >= 0
                // (&& R <= arr.length)
            }
        } else {
            // (inv &&) R - L == 1
            // =>
            // post:
            // func(y < R) == false && (if any) func(y >= R) == true && R >= 0
            // (&& R <= arr.length)
            return R;
        }
    }

    public static int recursiveSearch(int[] arr, BinarySearchInterface f) {
        // pre: none

        // f is monotonically increasing on [0 : arr.length)
        BinarySearchFunc func = new BinarySearchFunc(f);
        // func is monotonically increasing on [-1 : arr.length] &&
        //
        // func(-1) == false && func(arr.length) == true
        // <=>
        // func(L) == false && func(R) == true && R >= 0
        return inRecursive(arr, func, -1, arr.length);
        // post:
        // f(y < r) == false && (if any) f(y >= r) == true &&
        // r >= 0 && r <= arr.length
    }
}
