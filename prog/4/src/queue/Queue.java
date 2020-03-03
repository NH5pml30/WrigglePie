package queue;

public interface Queue extends Collection {
    // inv:
    // n from Collection &&
    // for all i = 1..n a[i] != null

    // Collection immutable <=> n = n' && for all i=1..n : a[i]' = a[i]

    // pre: element != null
    // post:
    // n = n' + 1 &&
    // for all i=1..n' : a[i]' = a[i] &&
    // a[n] = element
    void enqueue(Object element);

    // pre: n > 0
    // post: r = a[1] && immutable
    Object element();

    // pre: n > 0
    // post:
    // r = a[1]' && n = n' - 1 && for all i=1..n : a[i+1]' = a[i]
    Object dequeue();
}
