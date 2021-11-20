package queue;

import java.util.function.Function;
import java.util.function.Predicate;

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

    // pre: predicate != null
    // post:
    // r.n == sum(predicate(a[i])) && for any i predicate(r.a[i]) == true &&
    // for any i : predicate(a[i]) == true  exists i' : (r.a[i'] == a[i] &&
    //   for any j < i : predicate(a[j]) == true  j' < i')
    Queue filter(Predicate<Object> predicate);

    // pre: function != null, function(a[i]) != null
    // post:
    // r.n == n && r.a[i] == function(a[i])
    Queue map(Function<Object, Object> function);
}

