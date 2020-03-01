package queue;

import java.util.Arrays;

public class ArrayQueue {
    private int begin = 0, size = 0;
    private Object[] elements = new Object[5];

    // inv:
    // n >= 0 &&
    // for all i = 1..n a[i] != null

    // immutable <=> n = n' && for all i=1..n : a[i]' = a[i]

    // pre: 0 <= i < n
    // post: a[i+1] = obj &| otherwise immutable && r = a[i+1]'
    private Object set(int i, Object obj) {
        int at = (begin + i) % elements.length;
        Object res = elements[at];
        elements[at] = obj;
        return res;
    }

    // pre: 0 <= i < n
    // post: r = a[i+1] && immutable
    private Object get(int i) {
        return elements[(begin + i) % elements.length];
    }

    // pre: element != null
    // post:
    // n = n' + 1 &&
    // for all i=1..n' : a[i]' = a[i] &&
    // a[n] = element
    public void enqueue(Object element) {
        assert element != null;

        ensureCapacity(size + 1);
        set(size++, element);
    }

    // pre: none
    // post: immutable
    private void ensureCapacity(int capacity) {
        if (capacity > elements.length) {
            elements = unwrap(capacity * 2);
            begin = 0;
        }
    }

    // pre: element != null
    // post:
    // n = n' + 1 &&
    // for all i=1..n' : a[i]' = a[i+1] &&
    // a[1] = element
    public void push(Object element) {
        assert element != null;

        ensureCapacity(size + 1);
        begin = (begin - 1 + elements.length) % elements.length;
        set(0, element);
        size++;
    }

    // pre: newSize >= n
    // post: r = a && immutable
    private Object[] unwrap(int newSize) {
        Object[] unwrapped = Arrays.copyOfRange(elements, begin, newSize + begin);
        System.arraycopy(
            elements, 0,
            unwrapped, elements.length - begin, size - elements.length + begin
        );
        return unwrapped;
    }

    // pre: n > 0
    // post: r = a[1] && immutable
    public Object element() {
        assert size > 0;
        return get(0);
    }

    // pre: n > 0
    // post: r = a[n] && immutable
    public Object peek() {
        assert size > 0;
        return get(size - 1);
    }

    // pre: n > 0
    // post:
    // r = a[1]' && n = n' - 1 && for all i=1..n : a[i+1]' = a[i]
    public Object dequeue() {
        assert size > 0;

        Object res = set(0, null);
        begin = (begin + 1) % elements.length;
        size--;
        return res;
    }

    // pre: n > 0
    // post:
    // r = a[n']' && n = n' - 1 && for all i=1..n : a[i]' = a[i]
    public Object remove() {
        assert size > 0;

        return set(--size, null);
    }

    // pre: none
    // post: r = n && immutable
    public int size() {
        return size;
    }

    // pre: none
    // post: r = (n == 0) && immutable
    public boolean isEmpty() {
        return size == 0;
    }

    // pre: none
    // post: n = 0
    public void clear() {
        size = 0;
        Arrays.fill(elements, null);
    }
}
