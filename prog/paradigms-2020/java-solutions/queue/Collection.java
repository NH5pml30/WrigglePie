package queue;

public interface Collection {
    // inv: n >= 0

    // pre: none
    // post: n = 0
    void clear();

    // pre: none
    // post: r = n && immutable
    int size();

    // pre: none
    // post: r = (n == 0) && immutable
    boolean isEmpty();
}
