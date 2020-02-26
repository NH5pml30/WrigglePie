package search.util;

public interface BinarySearchInterface {
    // pre: atM is array element
    // post: apply is monotonically increasing on array
    // <=>
    // apply(y < atM) <= r &&
    // apply(y > atM) >= r
    boolean apply(int atM);
}
