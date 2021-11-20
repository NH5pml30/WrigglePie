package info.kgeorgiy.ja.holyavin.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.*;

public class ParallelMapperImpl implements ParallelMapper {
    private static class TaskQueue<T> {
        private final Queue<T> queue;

        TaskQueue(Supplier<Queue<T>> factory) {
            queue = factory.get();
        }

        TaskQueue() {
            this(ArrayDeque::new);
        }

        synchronized T poll() throws InterruptedException {
            while (queue.isEmpty()) {
                wait();
            }
            return queue.poll();
        }

        synchronized void offer(T el) throws InterruptedException {
            if (el == null) {
                throw new IllegalArgumentException("No nulls allowed");
            }
            queue.offer(el);
            notify();
        }
    }

    private static final class TaskHook {
        final CounterWrapper counter;
        RuntimeException exc = null;

        public TaskHook(CounterWrapper counter) {
            this.counter = counter;
        }
    }

    private static final class TaskPair {
        final Runnable runnable;
        final TaskHook owner;

        TaskPair(Runnable runnable, TaskHook owner) {
            this.runnable = runnable;
            this.owner = owner;
        }

        void setExc(RuntimeException exc) {
            this.owner.exc = exc;
        }

        void incrementCounter() {
            owner.counter.increment();
        }

        void run() {
            runnable.run();
        }
    }

    private final TaskQueue<TaskPair> tasks = new TaskQueue<>();
    private final ArrayList<Thread> factory = new ArrayList<>();

    public ParallelMapperImpl(int nThreads) {
        // :NOTE: no need for nThreads runnable
        Runnable runnable = () -> {
            try {
                while (!Thread.interrupted()) {
                    TaskPair task = tasks.poll();
                    try {
                        task.run();
                    } catch (RuntimeException exc) {
                        // :NOTE-2: first, catching Throwable and ignoring is super-bad (it includes Error)
                        // if the task throws, it should be delivered to the user, just like in streams
                        task.setExc(exc);
                    }
                    task.incrementCounter();
                }
            } catch (InterruptedException ignored) {
                // :NOTE-2: it's responsibility of the thread that decided to interrupt you,
                // so let them rule this out.
            } finally {
                Thread.currentThread().interrupt();
            }
        };
        for (int i = 0; i < nThreads; i++) {
            factory.add(new Thread(runnable));
            factory.get(i).start();
        }
    }

    @Override
    public <T, R> List<R> map(Function<? super T, ? extends R> function, List<? extends T> list)
            throws InterruptedException {
        final ArrayList<List<R>> storage = new ArrayList<>();
        final List<Runnable> tasksToOffer = Util.beginTasks(factory.size(), function, list, storage);
        final TaskHook hook = new TaskHook(new CounterWrapper(tasksToOffer.size()));

        for (Runnable task : tasksToOffer) {
            tasks.offer(new TaskPair(task, hook));
        }
        synchronized (hook.counter) {
            while (!hook.counter.finished()) {
                hook.counter.wait();
            }
        }
        if (hook.exc != null) {
            throw hook.exc;
        }
        return Util.endTasks(storage);
    }

    @Override
    public void close() {
        for (Thread thr : factory) {
            thr.interrupt();
        }
        Util.joinThreads(factory);
    }
}
