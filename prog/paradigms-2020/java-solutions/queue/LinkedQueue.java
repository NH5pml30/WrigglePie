package queue;

import java.util.Arrays;
import java.util.function.Consumer;

public class LinkedQueue extends AbstractQueue {
    private Node head = null, tail = null;

    @Override
    public void enqueueImpl(Object element) {
        tail = new Node(element, tail);
        if (size == 0) {
            head = tail;
        }
    }

    @Override
    public Object elementImpl() {
        return head.value;
    }

    @Override
    protected void remove() {
        head = head.next;
        if (size == 0) {
            tail = null;
        }
    }

    @Override
    public void clearImpl() {
        tail = head = null;
    }

    @Override
    protected Queue factory() {
        return new LinkedQueue();
    }

    @Override
    protected void traverse(Consumer<Object> action) {
        Node node = head;
        for (int i = 0; i < size; i++) {
            action.accept(node.value);
            node = node.next;
        }
    }

    private static class Node {
        private Object value;
        private Node next = null;

        public Node(Object value, Node prev) {
            assert value != null;

            this.value = value;
            if (prev != null) {
                prev.next = this;
            }
        }
    }
}
