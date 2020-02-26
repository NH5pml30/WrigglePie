package queue;

public class ArrayQueueADTTest {
    public static void fill(ArrayQueueADT queue) {
        for (int i = 0; i < 30; i++) {
            if (i % 2 == 1) {
                ArrayQueueADT.dequeue(queue);
            }
            ArrayQueueADT.enqueue(queue, i);
        }
    }

    public static void dump(ArrayQueueADT queue) {
        while (!ArrayQueueADT.isEmpty(queue)) {
            System.out.println(
                ArrayQueueADT.size(queue) + " " +
                ArrayQueueADT.element(queue) + " " +
                ArrayQueueADT.dequeue(queue)
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
