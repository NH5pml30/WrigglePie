package info.kgeorgiy.ja.holyavin.arrayset;

import java.util.*;

public class ArraySet<E> extends AbstractSet<E> implements NavigableSet<E> {
    private static class SortedImmutableArraySlice<E extends C, C> {
        private final Comparator<C> comparator;
        private final E[] data;
        private final boolean isDescending;
        private final int from, to;

        @SuppressWarnings("unchecked") // want to throw on cast fail
        private static <C> int elementsCompareNatural(C lhs, C rhs) {
            return ((Comparable<C>) lhs).compareTo(rhs); // pre: lhs, rhs not null
        }

        private static <C> int elementsCompare(C lhs, C rhs, Comparator<C> comparator) {
            return comparator == null ? elementsCompareNatural(lhs, rhs) : comparator.compare(lhs, rhs);
        }

        int elementsCompare(C lhs, C rhs) {
            return elementsCompare(lhs, rhs, comparator);
        }

        private static <C> int sortAndRemoveDuplicates(C[] data, Comparator<C> comparator) {
            if (data.length <= 1) {
                return data.length;
            }

            Arrays.sort(data, comparator);

            int j = 1;
            for (int i = 1; i < data.length; i++) {
                if (elementsCompare(data[i - 1], data[i], comparator) != 0) {
                    data[j++] = data[i];
                }
            }
            return j;
        }

        SortedImmutableArraySlice(E[] data, Comparator<C> comparator) {
            this(data, 0, sortAndRemoveDuplicates(data, comparator), comparator);
        }

        SortedImmutableArraySlice(E[] data, int from, int to, Comparator<C> comparator) {
            this.data = data;
            this.comparator = comparator;
            if (from <= to) {
                this.from = from;
                this.to = to;
                isDescending = false;
            } else {
                this.from = to + 1;
                this.to = from + 1;
                isDescending = true;
            }
        }

        private static int changeFound(int x) {
            return -x - 1;
        }

        private int changeToLocal(int x) {
            return isDescending ? to - x - 1 : x - from;
        }

        private int changeToGlobal(int x) {
            return isDescending ? to - x - 1 : x + from;
        }

        SortedImmutableArraySlice<E, C> descending() {
            return new SortedImmutableArraySlice<>(data, changeToGlobal(size() - 1), changeToGlobal(-1),
                    comparator);
        }

        static class IteratorEx<E> implements Iterator<E> {
            private final int to, delta;
            private int index;
            private final E[] dataRef;

            IteratorEx(int from, int to, E[] dataRef) {
                this.index = from;
                this.to = to;
                this.delta = from <= to ? 1 : -1;
                this.dataRef = dataRef;
            }

            @Override
            public boolean hasNext() {
                return index != to;
            }

            @Override
            public E next() {
                E res = dataRef[index];
                index += delta;
                return res;
            }
        }

        ;

        Iterator<E> createIterator(boolean change) {
            if (!(isDescending ^ change)) {
                return new IteratorEx<>(from, to, data);
            } else {
                return new IteratorEx<>(to - 1, from - 1, data);
            }
        }

        Iterator<E> iterator() {
            return createIterator(false);
        }

        Iterator<E> descendingIterator() {
            return createIterator(true);
        }

        int binarySearch(Object element) {
            @SuppressWarnings("unchecked") // want to throw ClassCastException on fail
            int res = Arrays.binarySearch(data, from, to, (C) element, comparator);
            int notFoundDelta = isDescending ? 1 : 0;
            return res < 0
                    ? changeFound(changeToLocal(changeFound(res)) + notFoundDelta)
                    : changeToLocal(res);
        }

        int size() {
            return to - from;
        }

        SortedImmutableArraySlice<E, C> subList(int from, int to) {
            if (from < 0) {
                throw new IndexOutOfBoundsException(from);
            } else if (to > size()) {
                throw new IndexOutOfBoundsException(to);
            } else if (from > to) {
                throw new IllegalArgumentException("Illegal range bounds");
            }
            return new SortedImmutableArraySlice<E, C>(data, changeToGlobal(from), changeToGlobal(to), comparator);
        }

        void assertNotEmpty() {
            if (size() == 0) {
                throw new NoSuchElementException();
            }
        }

        E first() {
            assertNotEmpty();
            return getAt(0);
        }

        E last() {
            assertNotEmpty();
            return getAt(size() - 1);
        }

        E getAt(int at) {
            return data[changeToGlobal(at)];
        }

        E boundedGetAt(int at) {
            return at >= 0 && at < size() ? getAt(at) : null;
        }

        Comparator<C> comparator() {
            return isDescending ? Collections.reverseOrder(comparator) : comparator;
        }
    }

    SortedImmutableArraySlice<E, ? super E> data;

    private ArraySet(SortedImmutableArraySlice<E, ? super E> data) {
        this.data = data;
    }

    public ArraySet() {
        this(new ArrayList<E>(0));
    }

    public ArraySet(Collection<? extends E> c) {
        this(c, null);
    }

    @SuppressWarnings("unchecked") // unchecked generic cast is ok here
    public ArraySet(Collection<? extends E> c, Comparator<? super E> comparator) {
        this(new SortedImmutableArraySlice<>((E[]) c.toArray(), comparator));
    }

    private int searchInd(E e, int foundDiff, int notFoundDiff) {
        int res = data.binarySearch(e);
        return res >= 0 ? res + foundDiff : SortedImmutableArraySlice.changeFound(res) + notFoundDiff;
    }

    private int lowerFloorInd(E e, boolean inclusive) {
        return searchInd(e, inclusive ? 0 : -1, -1);
    }

    private E lowerFloor(E e, boolean inclusive) {
        return data.boundedGetAt(lowerFloorInd(e, inclusive));
    }

    private int higherCeilingInd(E e, boolean inclusive) {
        return searchInd(e, inclusive ? 0 : 1, 0);
    }

    private E higherCeiling(E e, boolean inclusive) {
        return data.boundedGetAt(higherCeilingInd(e, inclusive));
    }

    @Override
    public E lower(E e) {
        return lowerFloor(e, false);
    }

    @Override
    public E floor(E e) {
        return lowerFloor(e, true);
    }

    @Override
    public E ceiling(E e) {
        return higherCeiling(e, true);
    }

    @Override
    public E higher(E e) {
        return higherCeiling(e, false);
    }

    @Override
    public E pollFirst() {
        throw new UnsupportedOperationException("Cannot poll first on unmodifiable set");
    }

    @Override
    public E pollLast() {
        throw new UnsupportedOperationException("Cannot poll last on unmodifiable set");
    }

    @Override
    public Iterator<E> iterator() {
        return data.iterator();
    }

    @Override
    public NavigableSet<E> descendingSet() {
        return new ArraySet<E>(data.descending());
    }

    @Override
    public Iterator<E> descendingIterator() {
        return data.descendingIterator();
    }

    private NavigableSet<E> subSetInd(int from, int to) {
        return new ArraySet<E>(data.subList(from, to));
    }

    @Override
    public NavigableSet<E> subSet(E fromElement, boolean fromInclusive, E toElement, boolean toInclusive) {
        int cmp = data.elementsCompare(fromElement, toElement);
        if (cmp > 0) {
            throw new IllegalArgumentException("Illegal range bounds");
        } else if (cmp == 0 && !(fromInclusive && toInclusive)) {
            return subSetInd(0, 0);
        }
        return subSetInd(higherCeilingInd(fromElement, fromInclusive), lowerFloorInd(toElement, toInclusive) + 1);
    }

    @Override
    public NavigableSet<E> headSet(E toElement, boolean inclusive) {
        return subSetInd(0, lowerFloorInd(toElement, inclusive) + 1);
    }

    @Override
    public NavigableSet<E> tailSet(E fromElement, boolean inclusive) {
        return subSetInd(higherCeilingInd(fromElement, inclusive), size());
    }

    @Override
    public int size() {
        return data.size();
    }

    @Override
    public boolean contains(Object o) {
        return data.binarySearch(o) >= 0;
    }

    @Override
    public Comparator<? super E> comparator() {
        return data.comparator();
    }

    @Override
    public SortedSet<E> subSet(E fromElement, E toElement) {
        return subSet(fromElement, true, toElement, false);
    }

    @Override
    public SortedSet<E> headSet(E toElement) {
        return headSet(toElement, false);
    }

    @Override
    public SortedSet<E> tailSet(E fromElement) {
        return tailSet(fromElement, true);
    }

    @Override
    public E first() {
        return data.first();
    }

    @Override
    public E last() {
        return data.last();
    }
}
