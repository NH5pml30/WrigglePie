package queue;

import java.util.Arrays;

public class ArrayQueueADT {
    private int begin = 0, size = 0;
    private Object[] elements = new Object[5];

    public static void enqueue(ArrayQueueADT queue, Object element) {
        assert element != null;

        ensureCapacity(queue, queue.size + 1);
        queue.elements[(queue.begin + queue.size++) % queue.elements.length] = element;
    }

    private static void ensureCapacity(ArrayQueueADT queue, int capacity) {
        if (capacity > queue.elements.length) {
            queue.elements = unwrap(queue, capacity * 2);
            queue.begin = 0;
        }
    }

    private static Object[] unwrap(ArrayQueueADT queue, int newSize) {
        Object[] unwrapped = Arrays.copyOfRange(queue.elements, queue.begin, newSize + queue.begin);
        System.arraycopy(
            queue.elements, 0,
            unwrapped, queue.elements.length - queue.begin,
            queue.size - queue.elements.length + queue.begin
        );
        return unwrapped;
    }

    public static Object element(ArrayQueueADT queue) {
        return queue.elements[queue.begin];
    }

    public static Object dequeue(ArrayQueueADT queue) {
        assert queue.size > 0;

        Object res = queue.elements[queue.begin];
        queue.elements[queue.begin] = null;
        queue.begin = (queue.begin + 1) % queue.elements.length;
        queue.size--;
        return res;
    }

    public static int size(ArrayQueueADT queue) {
        return queue.size;
    }

    public static boolean isEmpty(ArrayQueueADT queue) {
        return queue.size == 0;
    }

    public static void clear(ArrayQueueADT queue) {
        queue.begin = queue.size = 0;
        Arrays.fill(queue.elements, null);
    }
}
