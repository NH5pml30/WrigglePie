package queue;

public class ArrayQueueTest {
    public static void fill(ArrayQueue queue) {
        for (int i = 0; i < 30; i++) {
            if (i % 2 == 1) {
                queue.dequeue();
            }
            queue.enqueue(i);
        }
    }

    public static void dump(ArrayQueue queue) {
        while (!queue.isEmpty()) {
            System.out.println(queue.size() + " " +
                queue.element() + " " + queue.dequeue());
        }
    }

    public static void main(String[] args) {
        ArrayQueue Queue = new ArrayQueue();
        fill(Queue);
        dump(Queue);
        Queue.clear();
        dump(Queue);
    }
}
