package queue;

import java.util.Arrays;
import java.util.function.Consumer;

public class ArrayQueue extends AbstractQueue {
    private int begin = 0;
    private Object[] elements = new Object[5];

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

    @Override
    public void enqueueImpl(Object element) {
        ensureCapacity(size + 1);
        set(size, element);
    }

    // pre: none
    // post: immutable
    private void ensureCapacity(int capacity) {
        if (capacity > elements.length) {
            elements = unwrap(capacity * 2);
            begin = 0;
        }
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

    @Override
    public Object elementImpl() {
        return get(0);
    }

    @Override
    protected void remove() {
        set(0, null);
        begin = (begin + 1) % elements.length;
    }

    @Override
    public void clearImpl() {
        Arrays.fill(elements, null);
    }

    @Override
    protected Queue factory() {
        return new ArrayQueue();
    }

    @Override
    protected void traverse(Consumer<Object> action) {
        for (int i = 0; i < size; i++) {
            action.accept(get(i));
        }
    }
}