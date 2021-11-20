package info.kgeorgiy.ja.holyavin.concurrent;

import info.kgeorgiy.java.advanced.concurrent.AdvancedIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class IterativeParallelism implements AdvancedIP {
    private final ParallelMapper parallelMapper;

    public IterativeParallelism() { this(null); }

    public IterativeParallelism(ParallelMapper parallelMapper) {
        this.parallelMapper = parallelMapper;
    }

    private <T, R> List<R> parallelMap(int nThreads, final List<T> list,
                                       final Function<? super T, ? extends R> function) throws InterruptedException {
        if (list.isEmpty()) {
            return List.of();
        }

        if (parallelMapper != null) {
            return Util.endTasks(parallelMapper.map(
                    s -> s.map(function).collect(Collectors.toUnmodifiableList()),
                    Util.partitionList(nThreads, list))
            );
        }

        final ArrayList<List<R>> results = new ArrayList<>();
        List<Runnable> tasks = Util.beginTasks(nThreads, function, list, results);
        final ArrayList<Thread> threads = new ArrayList<>();
        {
            int i = 0;
            for (Runnable task : tasks) {
                threads.add(new Thread(task));
                threads.get(i).start();
                i++;
            }
        }
        Util.joinThreads(threads);
        if (Thread.interrupted()) {
            throw new InterruptedException();
        }
        return Util.endTasks(results);
    }

    private <T, R> List<R> parallelReduce(int nThreads, List<T> list, Function<Stream<T>, R> function)
            throws InterruptedException {
        return parallelMap(nThreads, Util.partitionList(nThreads, list), function);
    }

    private <T, R> R streamMapReduce(int threads, List<T> list, Function<T, R> mapper,
                                     Function<Stream<R>, R> reducer)
            throws InterruptedException {
        Function<Stream<T>, R> f = l -> reducer.apply(l.map(mapper));
        return reducer.apply(parallelReduce(threads, list, f).stream());
    }

    private <T> T streamReduce(int threads, List<T> list, Function<Stream<T>, T> reducer)
            throws InterruptedException {
        return streamMapReduce(threads, list, Function.identity(), reducer);
    }

    private <T, U> List<U> streamMap(int threads, List<T> list,
                                     final Function<Stream<T>, Stream<? extends U>> op)
            throws InterruptedException {
        Stream<? extends U> intermediate = parallelReduce(threads, list, op::apply)
                .stream().flatMap(Function.identity());
        return intermediate.collect(Collectors.toUnmodifiableList());
    }

    @Override
    public <T> T maximum(int threads, List<? extends T> list, Comparator<? super T> comparator)
            throws InterruptedException {
        return streamReduce(threads, list, s -> s.max(comparator).get());
    }

    @Override
    public <T> T minimum(int threads, List<? extends T> list, Comparator<? super T> comparator)
            throws InterruptedException {
        return maximum(threads, list, comparator.reversed());
    }

    private final static int CANCEL_CHECK_ITERS = 128;

    private <T> Iterable<T> getIterable(Stream<T> stream) {
        return stream::iterator;
    }

    @Override
    public <T> boolean all(int threads, List<? extends T> list, final Predicate<? super T> predicate)
            throws InterruptedException {
        final SettingFlag cancellationToken = new SettingFlag();
        return parallelReduce(threads, list, l -> {
            int iter = 0;
            for (T el : getIterable(l)) {
                if (iter % CANCEL_CHECK_ITERS == 0 && cancellationToken.get()) {
                    return false;
                }
                if (!predicate.test(el)) {
                    cancellationToken.set();
                    return false;
                }
                iter++;
            }
            return true;
        }).stream().allMatch(Predicate.isEqual(true));
    }

    @Override
    public <T> boolean any(int threads, List<? extends T> list, Predicate<? super T> predicate)
            throws InterruptedException {
        return !all(threads, list, predicate.negate());
    }

    @Override
    public <T, U> List<U> map(int threads, List<? extends T> list, final Function<? super T, ? extends U> function)
            throws InterruptedException {
        return streamMap(threads, list, s -> s.map(function));
    }

    @Override
    public <T> List<T> filter(int threads, List<? extends T> list, final Predicate<? super T> predicate)
            throws InterruptedException {
        return streamMap(threads, list, s -> s.filter(predicate));
    }

    @Override
    public String join(int threads, List<?> list) throws InterruptedException {
        return String.join("", map(threads, list, Objects::toString));
    }

    private <T> Function<Stream<T>, T> getMonoidReducer(Monoid<T> monoid) {
        return s -> s.reduce(monoid.getIdentity(), monoid.getOperator());
    }

    @Override
    public <T> T reduce(int threads, List<T> list, Monoid<T> monoid) throws InterruptedException {
        return streamReduce(threads, list, getMonoidReducer(monoid));
    }

    @Override
    public <T, R> R mapReduce(int threads, List<T> list, Function<T, R> function, Monoid<R> monoid)
            throws InterruptedException {
        return streamMapReduce(threads, list, function, getMonoidReducer(monoid));
    }
}
