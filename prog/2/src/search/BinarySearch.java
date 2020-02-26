package search;

import static search.util.BinarySearch.*;

public class BinarySearch extends BinarySearchBase {
    public static void main(String[] args) {
        if (!instance.read(args)) {
            return;
        }
        int res = iterativeSearch(instance.arr, y -> y <= instance.x);
        System.out.println(res);
    }
}
