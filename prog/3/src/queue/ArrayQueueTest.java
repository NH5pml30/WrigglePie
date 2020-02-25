package queue;

public class ArrayQueueTest {
    public static void fill(ArrayQueue Queue) {
        for (int i = 0; i < 10; i++) {
            Queue.enqueue(i);
        }
    }

    public static void dump(ArrayQueue Queue) {
        while (!Queue.isEmpty()) {
            System.out.println(Queue.size() + " " +
                Queue.element() + " " + Queue.dequeue());
        }
    }

    public static void main(String[] args) {
        ArrayQueue Queue = new ArrayQueue();
        fill(Queue);
        dump(Queue);
    }
}
