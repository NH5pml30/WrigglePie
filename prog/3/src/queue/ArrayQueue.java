package queue;

import java.util.Arrays;

public class ArrayQueue {
    private int begin = 0, size = 0;
    private Object[] elements = new Object[5];

    // inv:
    // n >= 0 &&
    // for all i = 1..n a[i] != null

    // immutable <=> n = n' && for all i=1..n : a[i]' = a[i]

    // pre: element != null
    // post:
    // n = n' + 1 &&
    // for all i=1..n' : a[i]' = a[i] &&
    // a[n] = element
    public void enqueue(Object element) {
        assert element != null;

        ensureCapacity(size + 1);
        elements[(begin + size++) % elements.length] = element;
    }

    private void ensureCapacity(int capacity) {
        if (capacity > elements.length) {
            elements = unwrap(capacity * 2);
            begin = 0;
        }
    }

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
        return elements[begin];
    }

    // pre: n > 0
    // post:
    // r = a[1]' && n = n' - 1 && for all i=1..n : a[i+1]' = a[i]
    public Object dequeue() {
        assert size > 0;

        Object res = elements[begin];
        elements[begin] = null;
        begin = (begin + 1) % elements.length;
        size--;
        return res;
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
