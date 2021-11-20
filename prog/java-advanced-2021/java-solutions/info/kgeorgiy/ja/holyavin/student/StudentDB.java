package info.kgeorgiy.ja.holyavin.student;

import java.util.*;
import java.util.function.*;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import info.kgeorgiy.java.advanced.student.*;

public class StudentDB implements AdvancedQuery {
    // :NOTE: constants should be in all capitals
    // :NOTE: extract common comparators into constants
    // :NOTE: Comparator.naturalOrder() does not create a new object, unlike String::compareTo
    private final static Comparator<Student> STUDENT_NAME_COMPARATOR =
            Comparator.comparing(Student::getLastName)
                    .thenComparing(Student::getFirstName).reversed()
                    .thenComparingInt(Student::getId);
    private final static Function<Student, String>
            STUDENT_FIRST_NAME_GETTER = Student::getFirstName,
            STUDENT_LAST_NAME_GETTER = Student::getLastName,
            STUDENT_FULL_NAME_GETTER =
                    student -> String.join(" ", student.getFirstName(), student.getLastName());
    private final static Function<Student, GroupName> STUDENT_GROUP_NAME_GETTER = Student::getGroup;

    private final static Supplier<TreeSet<String>> STRING_TREE_SET_FACTORY = TreeSet::new;
    private final static Function<TreeSet<String>, Set<String>> STRING_UNSET_FACTORY = Collections::unmodifiableSet;
    private final static BinaryOperator<String> STRING_MINIMUM = BinaryOperator.minBy(Comparator.naturalOrder());

    private final static Comparator<Group>
            GROUP_COMPARATOR = Comparator.comparing(Group::getName),
            GROUP_SIZE_COMPARATOR =
                    Comparator.comparing(compose(List::size, Group::getStudents)).thenComparing(Group::getName);
    private final static Function<Group, GroupName> GROUP_NAME_GETTER = Group::getName;
    private final static Function<Group, List<Student>> GROUP_STUDENTS_GETTER = Group::getStudents;


    private static <T> Collector<T, ?, List<T>> listCollector() {
        return Collectors.toUnmodifiableList();
    }
    private static <T> List<T> toList(Stream<T> stream) {
        return stream.collect(listCollector());
    }

    private <R, I> R getProperties(List<Student> students, Function<? super Student, ? extends I> getter,
                                   Collector<? super I, ?, R> collector) {
        return students.stream().map(getter).collect(collector);
    }

    private <R> List<R> getProperties(List<Student> students, Function<? super Student, ? extends R> getter) {
        return getProperties(students, getter, listCollector());
    }

    /** Returns student {@link Student#getFirstName() first names}. */
    @Override
    public List<String> getFirstNames(List<Student> students) {
        return getProperties(students, STUDENT_FIRST_NAME_GETTER);
    }

    /** Returns student {@link Student#getLastName() last names}. */
    @Override
    public List<String> getLastNames(List<Student> students) {
        return getProperties(students, STUDENT_LAST_NAME_GETTER);
    }

    /** Returns student {@link Student#getGroup() groups}. */
    @Override
    public List<GroupName> getGroups(List<Student> students) {
        return getProperties(students, STUDENT_GROUP_NAME_GETTER);
    }

    /** Returns full student name. */
    @Override
    public List<String> getFullNames(List<Student> students) {
        return getProperties(students, STUDENT_FULL_NAME_GETTER);
    }

    private final static Collector<String, ?, Set<String>> DISTINCT_FIRST_NAMES_COLLECTOR =
            Collectors.collectingAndThen(
                    Collectors.toCollection(STRING_TREE_SET_FACTORY), STRING_UNSET_FACTORY
            );
    /** Returns distinct student {@link Student#getFirstName() first names} in lexicographic order. */
    @Override
    public Set<String> getDistinctFirstNames(List<Student> students) {
        return getProperties(students, STUDENT_FIRST_NAME_GETTER, DISTINCT_FIRST_NAMES_COLLECTOR);
    }

    /** Returns a {@link Student#getFirstName() first name} of the student with maximal {@link Student#getId() id}. */
    @Override
    public String getMaxStudentFirstName(List<Student> students) {
        return students.stream().max(Comparator.naturalOrder()).map(STUDENT_FIRST_NAME_GETTER).orElse("");
    }

    /** Returns students ordered by {@link Student#getId() id}. */
    @Override
    public List<Student> sortStudentsById(Collection<Student> students) {
        return toList(students.stream().sorted());
    }

    private Stream<Student> orderedByName(Stream<Student> students) {
        return students.sorted(STUDENT_NAME_COMPARATOR);
    }

    /** Returns students ordered by name. */
    @Override
    public List<Student> sortStudentsByName(Collection<Student> students) {
        return toList(orderedByName(students.stream()));
    }

    // :NOTE: filter _before_ sorting
    private <R, I> R findBy(Collection<Student> students, Function<? super Student, ? extends I> transformer, I what,
                         Collector<? super Student, ?, R> collector) {
        return orderedByName(students.stream().filter(student -> transformer.apply(student).equals(what))).collect(collector);
    }

    private <I> List<Student> findBy(Collection<Student> students, Function<? super Student, ? extends I> transformer,
                                     I what) {
        return findBy(students, transformer, what, listCollector());
    }

    /** Returns students having specified first name. Students are ordered by name. */
    @Override
    public List<Student> findStudentsByFirstName(Collection<Student> students, String name) {
        return findBy(students, STUDENT_FIRST_NAME_GETTER, name);
    }

    /** Returns students having specified last name. Students are ordered by name. */
    @Override
    public List<Student> findStudentsByLastName(Collection<Student> students, String name) {
        return findBy(students, STUDENT_LAST_NAME_GETTER, name);
    }

    /** Returns students having specified groups. Students are ordered by name. */
    @Override
    public List<Student> findStudentsByGroup(Collection<Student> students, GroupName group) {
        return findBy(students, STUDENT_GROUP_NAME_GETTER, group);
    }

    private final static Collector<Student, ?, Map<String, String>> FIND_STUDENT_NAMES_BY_GROUP_COLLECTOR =
            Collectors.toUnmodifiableMap(
                    STUDENT_LAST_NAME_GETTER, STUDENT_FIRST_NAME_GETTER, STRING_MINIMUM
            );
    /** Returns map of group's student last names mapped to minimal first name. */
    @Override
    public Map<String, String> findStudentNamesByGroup(Collection<Student> students, GroupName group) {
        return findBy(students, STUDENT_GROUP_NAME_GETTER, group, FIND_STUDENT_NAMES_BY_GROUP_COLLECTOR);
    }

    private Stream<Group> getGroups(Collection<Student> students,
                                    Function<List<Student>, ? extends List<Student>> collectingAndThen) {
        return students.stream().collect(
                Collectors.groupingBy(
                        STUDENT_GROUP_NAME_GETTER,
                        Collectors.collectingAndThen(listCollector(), collectingAndThen)
                )
        ).entrySet().stream().map(entry -> new Group(entry.getKey(), entry.getValue()));
    }

    private Stream<Group> getGroups(Collection<Student> students) {
        return getGroups(students, Function.identity());
    }

    private List<Group> getGroupsSorted(Collection<Student> students,
                                        Function<List<Student>, ? extends List<Student>> collectingAndThen) {
        return toList(getGroups(students, collectingAndThen).sorted(GROUP_COMPARATOR));
    }

    /** Returns student groups, where both groups and students within a group are ordered by name. */
    @Override
    public List<Group> getGroupsByName(Collection<Student> students) {
        return getGroupsSorted(students, this::sortStudentsByName);
    }

    /** Returns student groups, where groups are ordered by name, and students within a group are ordered by id. */
    @Override
    public List<Group> getGroupsById(Collection<Student> students) {
        return getGroupsSorted(students, this::sortStudentsById);
    }

    private static <T, M, R> Function<T, R> compose(Function<? super M, R> lhs, Function<T, ? extends M> rhs) {
        return lhs.compose(rhs);
    }

    // :NOTE: try to extract something common from the following three functions
    /**
     * Returns group containing maximum number of students.
     * If there are more than one largest group, the one with greatest name is returned.
     */
    @Override
    public GroupName getLargestGroup(Collection<Student> students) {
        return getGroups(students)
                .max(GROUP_SIZE_COMPARATOR)
                .map(GROUP_NAME_GETTER).orElse(null);
    }

    private <R, V, T> R computeCharacteristicsAndFindMax(
            Stream<T> stream, Collector<? super T, ?, ? extends Map<R, V>> collector,
            Comparator<? super Map.Entry<R, V>> comparator, R orElse
    ) {
        return stream.collect(collector).entrySet().stream().max(comparator).map(Map.Entry::getKey).orElse(orElse);
    }

    private final static Comparator<Map.Entry<GroupName, Set<String>>> LARGEST_GROUP_FIRST_NAME_COMPARATOR =
            Comparator
                    .comparing(compose(Set::size, Map.Entry<GroupName, Set<String>>::getValue))
                    .thenComparing(Map.Entry::getKey, Comparator.reverseOrder());
    /**
     * Returns group containing maximum number of students with distinct first names.
     * If there are more than one largest group, the one with smallest name is returned.
     */
    @Override
    public GroupName getLargestGroupFirstName(Collection<Student> students) {
        return computeCharacteristicsAndFindMax(
                getGroups(students),
                Collectors.toMap(GROUP_NAME_GETTER, compose(this::getDistinctFirstNames, GROUP_STUDENTS_GETTER)),
                LARGEST_GROUP_FIRST_NAME_COMPARATOR, null
        );
    }

    private final static Comparator<Map.Entry<String, Set<GroupName>>> MOST_POPULAR_NAME_COMPARATOR =
            Comparator.comparing(compose(Set::size, Map.Entry<String, Set<GroupName>>::getValue))
                    .thenComparing(Map.Entry::getKey);
    private final static Collector<Student, ?, Map<String, Set<GroupName>>> MOST_POPULAR_NAME_COLLECTOR =
            Collectors.groupingBy(
                    STUDENT_FIRST_NAME_GETTER,
                    Collectors.mapping(STUDENT_GROUP_NAME_GETTER, Collectors.toUnmodifiableSet())
            );
    /**
     * Returns the first name of the student such that most number of groups has student with that name.
     * If there are more than one such name, the largest one is returned.
     */
    @Override
    public String getMostPopularName(Collection<Student> students) {
        return computeCharacteristicsAndFindMax(
                students.stream(), MOST_POPULAR_NAME_COLLECTOR,
                MOST_POPULAR_NAME_COMPARATOR, ""
        );
    }

    private <T> List<T> getPropertiesIndexed(Collection<Student> students, int[] ints,
                                             Function<? super Student, T> mapper) {
        return toList(Arrays.stream(ints).mapToObj((new ArrayList<>(students))::get).map(mapper));
    }

    /** Returns student {@link Student#getFirstName() first names} by indices. */
    @Override
    public List<String> getFirstNames(Collection<Student> students, int[] ints) {
        return getPropertiesIndexed(students, ints, STUDENT_FIRST_NAME_GETTER);
    }

    /** Returns student {@link Student#getLastName() last names} by indices. */
    @Override
    public List<String> getLastNames(Collection<Student> students, int[] ints) {
        return getPropertiesIndexed(students, ints, STUDENT_LAST_NAME_GETTER);
    }

    /** Returns student {@link Student#getGroup() groups} by indices. */
    @Override
    public List<GroupName> getGroups(Collection<Student> students, int[] ints) {
        return getPropertiesIndexed(students, ints, STUDENT_GROUP_NAME_GETTER);
    }

    /** Returns full student name by indices. */
    @Override
    public List<String> getFullNames(Collection<Student> students, int[] ints) {
        return getPropertiesIndexed(students, ints, STUDENT_FULL_NAME_GETTER);
    }
}
