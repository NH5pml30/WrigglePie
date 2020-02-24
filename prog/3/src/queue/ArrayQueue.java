package queue;

import java.util.Arrays;

public class ArrayQueue {
    private int begin = 0, size = 0;
    private Object[] elements = new Object[5];
    // inv:
    // elements.length >= size >= 0 &&
    // elements.length >= 5 &&
    // elements.length > begin >= 0 &&
    // for all i 0..size-1: elements[(begin + i) % elements.length] -- (i+1)-th element in queued order &&
    // for all i size..elements.length elements[(begin + i) % elements.length] == null

    public void enqueue(Object element) {
        // pre: elements != null
        assert element != null;

        ensureCapacity(size + 1);
        elements[(begin + size++) % elements.length] = element;
        // post:
        // size -> size + 1 &&
        // (size-1)-th element in queued order -- element
    }

    private void ensureCapacity(int capacity) {
        // pre: none
        while (capacity > elements.length) {
            Object[] newElements = Arrays.copyOfRange(elements, begin, elements.length * 2);
            System.arraycopy(
                    elements, 0,
                    newElements, elements.length - begin, size
            );
            begin = 0;
            elements = newElements;
        }
        // post:
        // elements.length >= capacity
    }

    public Object element() {
        // pre: size > 0
        assert size > 0;
        return elements[begin];
        // post: r -- first element in queued order && size' == size > 0
    }

    public Object dequeue() {
        // pre: size > 0
        assert size > 0;

        Object res = elements[begin];
        elements[begin] = null;
        begin = (begin + 1) % elements.length;
        size--;
        return res;
        // post: r -- previously first element in queued order && size' == size - 1
    }

    public int size() {
        // pre: none
        return size;
        // post: r = size
    }

    public boolean isEmpty() {
        // pre: none
        return size == 0;
        // post: r == (size == 0)
    }

    public void clear() {
        // pre: none
        size = 0;
        Arrays.fill(elements, null);
        // post: size = 0
    }
}
