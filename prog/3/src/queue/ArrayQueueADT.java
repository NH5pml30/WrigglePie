package queue;

import java.util.Arrays;

public class ArrayQueueADT {
    private int begin = 0, size = 0;
    private Object[] elements = new Object[5];

    // inv:
    // n >= 0 &&
    // for all i = 1..n a[i] != null

    // immutable <=> n = n' && for all i=1..n : a[i]' = a[i]

    // pre: 0 <= i < n
    // post: a[i+1] = obj &| otherwise immutable && r = a[i+1]'
    private static Object set(ArrayQueueADT queue, int i, Object obj) {
        int at = (queue.begin + i) % queue.elements.length;
        Object res = queue.elements[at];
        queue.elements[at] = obj;
        return res;
    }

    // pre: 0 <= i < n
    // post: r = a[i+1] && immutable
    private static Object get(ArrayQueueADT queue, int i) {
        return queue.elements[(queue.begin + i) % queue.elements.length];
    }

    // pre: element != null
    // post:
    // n = n' + 1 &&
    // for all i=1..n' : a[i]' = a[i] &&
    // a[n] = element
    public static void enqueue(ArrayQueueADT queue, Object element) {
        assert element != null;

        ensureCapacity(queue, queue.size + 1);
        set(queue, queue.size++, element);
    }

    // pre: none
    // post: immutable
    private static void ensureCapacity(ArrayQueueADT queue, int capacity) {
        if (capacity > queue.elements.length) {
            queue.elements = unwrap(queue, capacity * 2);
            queue.begin = 0;
        }
    }

    // pre: element != null
    // post:
    // n = n' + 1 &&
    // for all i=1..n' : a[i]' = a[i+1] &&
    // a[1] = element
    public static void push(ArrayQueueADT queue, Object element) {
        assert element != null;

        ensureCapacity(queue, queue.size + 1);
        queue.begin = (queue.begin - 1 + queue.elements.length) % queue.elements.length;
        set(queue, 0, element);
        queue.size++;
    }

    // pre: newSize >= n
    // post: r = a && immutable
    private static Object[] unwrap(ArrayQueueADT queue, int newSize) {
        Object[] unwrapped = Arrays.copyOfRange(queue.elements, queue.begin, newSize + queue.begin);
        System.arraycopy(
                queue.elements, 0,
                unwrapped, queue.elements.length - queue.begin, queue.size - queue.elements.length + queue.begin
        );
        return unwrapped;
    }

    // pre: n > 0
    // post: r = a[1] && immutable
    public static Object element(ArrayQueueADT queue) {
        assert queue.size > 0;
        return get(queue, 0);
    }

    // pre: n > 0
    // post: r = a[n] && immutable
    public static Object peek(ArrayQueueADT queue) {
        assert queue.size > 0;
        return get(queue, queue.size - 1);
    }

    // pre: n > 0
    // post:
    // r = a[1]' && n = n' - 1 && for all i=1..n : a[i+1]' = a[i]
    public static Object dequeue(ArrayQueueADT queue) {
        assert queue.size > 0;

        Object res = set(queue, 0, null);
        queue.begin = (queue.begin + 1) % queue.elements.length;
        queue.size--;
        return res;
    }

    // pre: n > 0
    // post:
    // r = a[n']' && n = n' - 1 && for all i=1..n : a[i]' = a[i]
    public static Object remove(ArrayQueueADT queue) {
        assert queue.size > 0;

        return set(queue, --queue.size, null);
    }

    // pre: none
    // post: r = n && immutable
    public static int size(ArrayQueueADT queue) {
        return queue.size;
    }

    // pre: none
    // post: r = (n == 0) && immutable
    public static boolean isEmpty(ArrayQueueADT queue) {
        return queue.size == 0;
    }

    // pre: none
    // post: n = 0
    public static void clear(ArrayQueueADT queue) {
        queue.size = 0;
        Arrays.fill(queue.elements, null);
    }
}
