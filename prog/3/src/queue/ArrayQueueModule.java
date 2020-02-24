package queue;

import java.util.Arrays;

public class ArrayQueueModule {
    private static int begin = 0, size = 0;
    private static Object[] elements = new Object[5];

    public static void enqueue(Object element) {
        assert element != null;

        ensureCapacity(size + 1);
        elements[(begin + size++) % elements.length] = element;
    }

    private static void ensureCapacity(int capacity) {
        if (capacity > elements.length) {
            Object[] newElements = Arrays.copyOfRange(elements, begin, elements.length * 2);
            System.arraycopy(
                    elements, 0,
                    newElements, elements.length - begin, size
            );
            begin = 0;
            elements = newElements;
        }
    }

    public static Object element() {
        return elements[begin];
    }

    public static Object dequeue() {
        assert size > 0;

        Object res = elements[begin];
        elements[begin] = null;
        begin = (begin + 1) % elements.length;
        size--;
        return res;
    }

    public static int size() {
        return size;
    }

    public static boolean isEmpty() {
        return size == 0;
    }

    public static void clear() {
        begin = size = 0;
        Arrays.fill(elements, null);
    }
}
