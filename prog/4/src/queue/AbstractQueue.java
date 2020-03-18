package queue;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

public abstract class AbstractQueue implements Queue {
    protected int size;

    @Override
    public void enqueue(Object element) {
        assert element != null;

        enqueueImpl(element);
        size++;
    }

    protected abstract void enqueueImpl(Object element);

    @Override
    public Object element() {
        assert size > 0;

        return elementImpl();
    }

    protected abstract Object elementImpl();

    @Override
    public Object dequeue() {
        assert size > 0;

        Object result = element();
        size--;
        remove();
        return result;
    }

    protected abstract void remove();

    public int size() {
        return size;
    }

    public boolean isEmpty() {
        return size == 0;
    }

    public void clear() {
        size = 0;
        clearImpl();
    }

    protected abstract void clearImpl();

    protected abstract Queue factory();

    // pre: function != null
    // pre: predicate != null
    // post:
    // r.n == sum(function(a[i]) != null ? 1 : 0) &&
    // for any i : function(a[i]) != null  exists i' : (r.a[i'] == function(a[i]) &&
    //   for any j < i : function(a[j]) != null  j' < i')
    private Queue filerMap(Function<Object, Object> function) {
        Queue res = factory();
        traverse((Object value) -> {
            Object x = function.apply(value);
            if (x != null) {
                res.enqueue(x);
            }
        });
        return res;
    }

    @Override
    public Queue filter(Predicate<Object> predicate) {
        assert predicate != null;
        return filerMap(x -> predicate.test(x) ? x : null);
    }

    @Override
    public Queue map(Function<Object, Object> function) {
        assert function != null;
        return filerMap(function);
    }

    protected abstract void traverse(Consumer<Object> action);
}
