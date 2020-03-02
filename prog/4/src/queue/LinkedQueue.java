package queue;

public class LinkedQueue extends AbstractQueue {
    private Node head, tail;

    @Override
    public void enqueueImpl(Object element) {
        tail = new Node(element, tail);
        if (isEmpty()) {
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
        if (isEmpty()) {
            tail = null;
        }
    }

    @Override
    public void clearImpl() {
        tail = head = null;
    }

    private static class Node {
        private Object value;
        private Node next;

        public Node(Object value, Node prev) {
            assert value != null;

            this.value = value;
            if (prev != null) {
                prev.next = this;
            }
        }
    }
}
