package info.kgeorgiy.ja.holyavin.concurrent;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class CounterWrapper {
    private volatile int counter = 0;
    private final int end;

    CounterWrapper(int end) {
        this.end = end;
    }

    int get() {
        return counter;
    }

    boolean finished() {
        return counter >= end;
    }

    synchronized void increment() {
        ++counter;
        if (counter == end) {
            notifyAll();
        }
    }
}

class SettingFlag {
    private volatile boolean val = false;

    boolean get() {
        return val;
    }

    void set() {
        val = true;
    }
}

class Util {
    public static <T> List<Stream<T>> partitionList(int nParts_, final List<T> list) {
        final int nParts = Math.max(1, Math.min(list.size(), nParts_));
        final int perPart = list.size() / nParts;
        int remainder = list.size() - nParts * perPart; // <= nParts
        List<Stream<T>> res = new ArrayList<>();

        // :NOTE: spread excessive tasks over all threads instead of just one
        int last = 0;
        for (int i = 0; i < nParts; i++) {
            int end = i < remainder ? last + perPart + 1 : last + perPart;
            res.add(list.subList(last, end).stream());
            last = end;
        }
        return res;
    }

    public static <T, R> List<Runnable> beginTasks(int nThreads,
                                                   final Function<? super T, ? extends R> function,
                                                   final List<? extends T> list,
                                                   final ArrayList<List<R>> storage) {
        final List<? extends Stream<? extends T>> partitioned = partitionList(nThreads, list);
        storage.clear();
        storage.addAll(Collections.nCopies(partitioned.size(), null));

        // :NOTE: absolutely no need for IntConsumer
        final ArrayList<Runnable> tasks = new ArrayList<>();
        {
            int i = 0;
            for (final Stream<? extends T> sublist : partitioned) {
                final int index = i;
                tasks.add(() -> {
                    List<R> res = sublist.map(function).collect(Collectors.toUnmodifiableList());
                    storage.set(index, res);
                });
                i++;
            }
        }
        return tasks;
    }

    public static <R> List<R> endTasks(final List<List<R>> storage) {
        return storage.stream().flatMap(List::stream).collect(Collectors.toUnmodifiableList());
    }

    public static void joinThreads(ArrayList<Thread> threads) {
        boolean interrupted = false;
        // :NOTE: it's more efficient to interrupt threads before waiting for them (if you got interrupted)
        int i;
        for (i = 0; i < threads.size(); i++) {
            while (true) {
                try {
                    threads.get(i).join();
                    break;
                } catch (InterruptedException cause) {
                    if (!interrupted) {
                        for (int j = i; j < threads.size(); j++) {
                            threads.get(j).interrupt();
                        }
                    }
                    interrupted = true;
                }
            }
        }
        if (interrupted) {
            Thread.currentThread().interrupt();
        }
    }
}
