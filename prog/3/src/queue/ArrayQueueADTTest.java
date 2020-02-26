package queue;

public class ArrayQueueADTTest {
    public static void fill(ArrayQueueADT Queue) {
        for (int i = 0; i < 10; i++) {
            ArrayQueueADT.enqueue(Queue, i);
        }
    }

    public static void dump(ArrayQueueADT Queue) {
        while (!ArrayQueueADT.isEmpty(Queue)) {
            System.out.println(
                ArrayQueueADT.size(Queue) + " " +
                ArrayQueueADT.element(Queue) + " " +
                ArrayQueueADT.dequeue(Queue)
            );
        }
    }

    public static void main(String[] args) {
        ArrayQueueADT Queue = new ArrayQueueADT();
        fill(Queue);
        dump(Queue);
        ArrayQueueADT.clear(Queue);
        dump(Queue);
    }
}
