package info.kgeorgiy.ja.holyavin.implementor;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.JarImpler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.*;
import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.CodeSource;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.jar.JarOutputStream;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;

/**
 * {@link Impler} implementation class.
 */
public class Implementor implements Impler, JarImpler {
    /**
     * Class for handling java code output.
     */
    private static class Output implements AutoCloseable
    {
        /**
         * Filter to write ascii representation of unicode symbols into file.
         */
        private static class Native2AsciiWriter extends FilterWriter {
            /**
             * Create a new filtered writer.
             *
             * @param out a Writer object to provide the underlying stream.
             * @throws NullPointerException if {@code out} is {@code null}
             */
            Native2AsciiWriter(BufferedWriter out) {
                super(out);
            }

            /**
             * Writes a single character.
             *
             * @param c int specifying a character to be written.
             * @throws IOException If an I/O error occurs.
             */
            @Override
            public void write(int c) throws IOException {
                if (c >= 128) {
                    super.write("\\u" + String.format("%04X", c));
                } else {
                    super.write(c);
                }
            }

            /**
             * Writes a portion of an array of characters.
             *
             * @param cbuf Buffer of characters to be written.
             * @param off Offset from which to start reading characters.
             * @param len Number of characters to be written.
             * @throws IndexOutOfBoundsException If the values of the {@code off} and {@code len} parameters cause the
             * array access to throw an {@code IndexOutOfBoundsException}.
             * @throws IOException If an I/O error occurs.
             */
            @Override
            public void write(char[] cbuf, int off, int len) throws IOException {
                for (int i = 0; i < len; i++) {
                    write(cbuf[off + i]);
                }
            }

            /**
             * Writes a portion of a string.
             *
             * @param str String to be written.
             * @param off Offset from which to start reading characters.
             * @param len Number of characters to be written.
             * @throws IndexOutOfBoundsException If the values of the {@code off} and {@code len} parameters cause the
             * string access to throw an {@code IndexOutOfBoundsException}.
             * @throws IOException If an I/O error occurs.
             */
            @Override
            public void write(String str, int off, int len) throws IOException {
                for (int i = 0; i < len; i++) {
                    write(str.charAt(off + i));
                }
            }

            /**
             * Writes a line separator. The line separator string is defined by the system property
             * {@code line.separator}, and is not necessarily a single newline ({@code '\n'}) character.
             *
             * @throws IOException If an I/O error occurs
             */
            void newLine() throws IOException {
                write(System.lineSeparator());
            }
        }

        /**
         * Buffered filtered writer for code output.
         */
        private final Native2AsciiWriter writer;
        /**
         * Current indent level in tabs (non-negative integer).
         */
        private int indent;
        /**
         * Path to the resulting file.
         */
        Path pOut;

        /**
         * Creates a handler for java code output with specified path, package and name.
         *
         * @param path Path to root directory.
         * @param packageName Resulting class package name.
         * @param className Resulting class name.
         * @throws IOException If an I/O error occurs.
         */
        Output(Path path, String packageName, String className) throws IOException {
            pOut = Path.of(path.toString(), packageName.split("\\.")).resolve(className + ".java");
            Path containingDirPath = pOut.getParent();
            if (containingDirPath != null) {
                Files.createDirectories(containingDirPath);
            }
            this.writer = new Native2AsciiWriter(Files.newBufferedWriter(pOut));
        }

        /**
         * Changes current indent level (in tabs).
         *
         * @param delta Change in indent level.
         * @throws IllegalArgumentException If resulting indent is negative.
         */
        void changeIndentLevel(int delta) {
            if (indent + delta < 0) {
                throw new IllegalArgumentException("Cannot have negative indent");
            }
            indent += delta;
        }


        /**
         * Writes input into file line-by-line (with line separators inserted).
         *
         * @param lines Input text lines.
         * @throws IOException If an I/O error occurs.
         */
        void writeln(String ...lines) throws IOException {
            writer.write("\t".repeat(indent));
            for (String str : lines) {
                writer.write(str);
            }
            writer.newLine();
        }

        /**
         * Closes the handler and releases any system resources associated with it.
         * <p>
         * Once the handler has been closed, further {@link #writeln(String...)} invocations will throw an IOException.
         * Closing a previously closed handler has no effect.
         *
         * @throws IOException If an I/O error occurs.
         */
        @Override
        public void close() throws IOException {
            writer.close();
        }

        /**
         * Calculates string access modifier representation for implementation from modifier flag collection.
         * <p>
         * Access modifier representation is "public" if modifiers include {@code public},
         * "protected" if otherwise modifiers include {@code protected}, and empty string in other cases.
         *
         * @param modifiers Modifiers.
         * @return Access modifier string representation.
         */
        private static String getAccessModifier(int modifiers) {
            return Modifier.isPublic(modifiers) ? "public" :
                    Modifier.isProtected(modifiers) ? "protected" : "";
        }

        /**
         * Builds string, containing java code for returning of default value of specified class.
         * <p>
         * Default value is {@code false} for {@code boolean}, {@code 0} for other primitive types,
         * and {@code null} for objects.
         *
         * @param returnType Type to build default return construction for.
         * @return Resulting string.
         */
        private static String getReturnValueConstruction(Class<?> returnType) {
            return returnType.equals(void.class) ? "" :
                    "return " + (returnType.isPrimitive()
                            ? returnType.equals(boolean.class) ? "false" : "0"
                            : "null") + ";";
        }

        /**
         * Maps stream with a function, returned from {@code indexedMapper} by supplying current iteration 0-based index.
         *
         * @param stream Stream to map.
         * @param indexedMapper Mapping function builder, consuming current iteration index.
         * @param <R> Result stream contained type.
         * @param <T> Initial stream contained type.
         * @return Mapped stream.
         */
        private static <R, T> Stream<R> mapWithIndex(Stream<T> stream,
                                                     IntFunction<Function<? super T, ? extends R>> indexedMapper) {
            final AtomicInteger n = new AtomicInteger(0);
            return stream.map(obj -> indexedMapper.apply(n.getAndIncrement()).apply(obj));
        }

        /**
         * Build java code string representation of a method's argument list.
         * <p>
         * Each argument in list is converted to its class name with {@link Class#getCanonicalName()},
         * prepended to a unique argument name, and joined into a list with commas. Note that generic types are left raw.
         *
         * @param method Method to process.
         * @return Code string.
         */
        private static String getArguments(Executable method) {
            return mapWithIndex(
                    Arrays.stream(method.getParameterTypes()),
                    n -> clazz -> clazz.getCanonicalName() + " arg" + n
            ).collect(Collectors.joining(", "));
        }

        /**
         * Returns second string if first is non-empty, otherwise returns empty string.
         *
         * @param toCheck String to check emptiness for.
         * @param str String to potentially return.
         * @return {@code str} if {@code toCheck} is non-empty, empty string otherwise.
         */
        static String stringIfNotEmpty(String toCheck, String str) {
            return toCheck.isEmpty() ? "" : str;
        }

        /**
         * Returns string consisting of space character if consumed string is non-empty, empty string otherwise.
         *
         * @param str String to check.
         * @return String of a single space character if {@code str} is non-empty, empty string otherwise.
         */
        static String spaceIfNotEmpty(String str) {
            return stringIfNotEmpty(str, " ");
        }

        /**
         * Constructs specified method's implementation java code and writes it to a file.
         *
         * @param annotation Annotation string representation (can be an empty string).
         * @param accessModifier Access modifier string representation (can be an empty string).
         * @param returnType Return type name string representation.
         * @param name Method name string representation (can be an empty string).
         * @param arguments Argument list string representation (can be an empty string).
         * @param excepts List of checked exceptions that a method can throw string representation (can be an empty string).
         * @param body Methods body string representation (can be an empty string).
         * @throws UncheckedIOException If an I/O error occurs.
         */
        private void implementMethod(String annotation, String accessModifier, String returnType,
                                     String name, String arguments, String excepts,
                                     String body) throws UncheckedIOException {
            try {
                if (!annotation.isEmpty()) {
                    writeln(annotation);
                }
                writeln(accessModifier, spaceIfNotEmpty(accessModifier),
                        returnType, spaceIfNotEmpty(name), name,
                        "(", arguments, ") ",
                        stringIfNotEmpty(excepts, "throws "), excepts, spaceIfNotEmpty(excepts), "{");
                changeIndentLevel(1);
                if (!body.isEmpty()) {
                    writeln(body);
                }
                changeIndentLevel(-1);
                writeln("}");
            } catch (IOException exc) {
                throw new UncheckedIOException(exc);
            }
        }

        /**
         * Constructs specified method implementation java code and writes it to a file.
         * <p>
         * Note that generic types are left raw. Implementation is created with the same access modifier and
         * an {@code @Override} annotation. No exceptions are specified as being potentially thrown.
         * The body of the method contains only default return value construction, if any.
         *
         * @param method Method to implement.
         * @throws UncheckedIOException If an I/O error occurs.
         */
        void implementMethod(Method method) throws UncheckedIOException {
            implementMethod(
                    "@Override",
                    getAccessModifier(method.getModifiers()),
                    method.getReturnType().getCanonicalName(),
                    method.getName(), getArguments(method), "",
                    getReturnValueConstruction(method.getReturnType()));
        }

        /**
         * Returns java code string representation of a call to a superclass constructor with forwarded arguments.
         * <p>
         * Arguments are written in the form of string "arg", prepended to its 0-based index in left-to-write order.
         *
         * @param init Constructor to call.
         * @return Java code string representation.
         */
        private static String getSuperConstruction(Constructor<?> init) {
            return "super(" +
                    mapWithIndex(
                            Collections.nCopies(init.getParameterCount(), "arg").stream(),
                            n -> str -> str + n
                    ).collect(Collectors.joining(", ")) + ");";
        }

        /**
         * Returns string representation of a list of exceptions that can be thrown from a specified constructor.
         * <p>
         * Each exception is converted to its class string representation with {@link Class#getCanonicalName()} and joined
         * into a list with commas.
         *
         * @param init Constructor to check.
         * @return Returns string representation of a list of exceptions that can be thrown from a specified constructor.
         */
        private static String getSuperExceptions(Constructor<?> init) {
            return Arrays.stream(init.getExceptionTypes())
                    .map(Class::getCanonicalName).collect(Collectors.joining(", "));
        }

        /**
         * Constructs specified constructor implementation java code and writes it to a file.
         * <p>
         * Note that generic types are left raw. Implementation is created with the a public access modifier.
         * The same exceptions are specified as being potentially thrown. The body contains only a call to {@code super}
         * constructor with forwarded arguments, if any.
         *
         * @param name Class name.
         * @param init Super constructor.
         * @throws UncheckedIOException If an I/O error occurs.
         */
        void implementConstructor(String name, Constructor<?> init) throws UncheckedIOException {
            if (init.getParameterCount() > 0) {
                implementMethod("",
                        getAccessModifier(Modifier.PUBLIC),
                        name, "", getArguments(init), getSuperExceptions(init),
                        getSuperConstruction(init));
            }
        }

        void implementConstructors(String name, Stream<Constructor<?>> constructors) throws UncheckedIOException {
            constructors.forEach(init -> implementConstructor(name, init));
        }

        void implementMethods(Stream<Method> methods) throws UncheckedIOException {
            methods.forEach(this::implementMethod);
        }
    }


    /**
     * File visitor to delete directory of files.
     */
    private static final SimpleFileVisitor<Path> DELETE_VISITOR = new SimpleFileVisitor<>() {
        @Override
        public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
        }

        @Override
        public FileVisitResult postVisitDirectory(final Path dir, final IOException exc) throws IOException {
            Files.delete(dir);
            return FileVisitResult.CONTINUE;
        }
    };

    /**
     * Collects into a stream potentially implementable methods from class {@code clazz}.
     * <p>
     * Potentially implementable abstract class methods are abstract non-private methods,
     * found in specified class and its superclasses. If the class is an interface, empty stream is returned.
     *
     * @param clazz Class to collect methods from.
     * @return Stream, containing potentially implementable methods from specified class.
     */
    private static Stream<Method> getImplementableAbstractClassMethods(Class<?> clazz) {
        if (clazz.isInterface()) {
            return Stream.empty();
        }

        class Signature {
            private final String name;
            private final Class<?>[] paramTypes;

            public Signature(String name, Class<?>[] paramTypes) {
                this.name = name;
                this.paramTypes = paramTypes;
            }

            @Override
            public int hashCode() {
                return 31 * name.hashCode() + Arrays.hashCode(paramTypes);
            }

            @Override
            public boolean equals(Object other) {
                if (other == this) {
                    return true;
                }
                if (other instanceof Signature) {
                    Signature s = (Signature) other;
                    return name.equals(s.name) && Arrays.equals(paramTypes, s.paramTypes);
                }
                return super.equals(other);
            }
        }

        class MethodId {
            private final Method method;
            private final int subclassRank;

            MethodId(Method method, int subclassRank) {
                this.method = method;
                this.subclassRank = subclassRank;
            }

            Signature getSignature() {
                return new Signature(method.getName(), method.getParameterTypes());
            }

            int getSubclassRank() {
                return subclassRank;
            }

            public Method getMethod() {
                return method;
            }
        }

        Stream<MethodId> res = Stream.empty();
        int rank = 0;
        while (clazz != null) {
            final int localRank = rank++;
            res = Stream.concat(res, Arrays.stream(clazz.getDeclaredMethods()).filter(
                    method -> !Modifier.isPrivate(method.getModifiers())
            ).map(method -> new MethodId(method, localRank)));
            clazz = clazz.getSuperclass();
        }

        return res.collect(
                Collectors.toMap(
                        MethodId::getSignature, Function.identity(),
                        BinaryOperator.minBy(Comparator.comparingInt(MethodId::getSubclassRank))
                )
        ).values().stream()
                .map(MethodId::getMethod)
                .filter(method -> Modifier.isAbstract(method.getModifiers()));
    }

    /**
     * Produces code implementing class or interface specified by provided {@code aClass}.
     * <p>
     * Generated class classes name are the same as classes name of the type token with {@code Impl} suffix
     * added. Generated source code are placed in the correct subdirectory of the specified
     * {@code root} directory and have correct file name. For example, the implementation of the
     * interface {@link java.util.List} would go to {@code $root/java/util/ListImpl.java}
     *
     *
     * @param aClass type token to create implementation for (non-null).
     * @param path root directory.
     * @throws ImplerException when implementation cannot be generated (non-null).
     * @return Path to result file.
     */
    private static Path implementEx(Class<?> aClass, Path path) throws ImplerException {
        int classModifiers = aClass.getModifiers();
        Constructor<?>[] usableConstructors =
                Arrays.stream(aClass.getDeclaredConstructors())
                        .filter(init -> !Modifier.isPrivate(init.getModifiers())).toArray(Constructor[]::new);
        if (aClass.isPrimitive() ||
                aClass.isEnum() || aClass.equals(Enum.class) ||
                aClass.isLocalClass() ||
                (!aClass.isInterface() && usableConstructors.length == 0) ||
                Modifier.isFinal(classModifiers) ||
                (aClass.isMemberClass() && Modifier.isPrivate(classModifiers))) {
            throw new ImplerException(
                    "Primitive, local, final, private nested types, " +
                            "classes with only private constructors, and enums not supported"
            );
        }

        String name = aClass.getSimpleName() + "Impl";
        Path res;
        try (Output output = new Output(path, aClass.getPackageName(), name)) {
            res = output.pOut;
            output.writeln(Output.stringIfNotEmpty(aClass.getPackageName(),
                    "package " + aClass.getPackageName() + ";"));
            output.writeln();
            output.writeln("public class ", name, " ",
                    aClass.isInterface() ? "implements " : "extends ", aClass.getCanonicalName(), " {");
            output.changeIndentLevel(1);
            try {
                output.implementConstructors(name, Arrays.stream(usableConstructors));
                output.implementMethods(
                        Stream.concat(
                                getImplementableAbstractClassMethods(aClass)
                                        .filter(method -> !Modifier.isPublic(method.getModifiers())),
                                Arrays.stream(aClass.getMethods())
                                        .filter(method -> {
                                            int modifiers = method.getModifiers();
                                            return Modifier.isAbstract(modifiers) &&
                                                    !Modifier.isPrivate(modifiers) && !method.isDefault();
                                        })
                        )
                );
            } catch (UncheckedIOException exc) {
                throw exc.getCause();
            }
            output.changeIndentLevel(-1);
            output.writeln("}");
            output.writeln();
        } catch (IOException exc) {
            throw new ImplerException("Error occurred during writing of result file", exc);
        }
        return res;
    }

    /**
     * Produces code implementing class or interface specified by provided {@code aClass}.
     * <p>
     * Generated class classes name are the same as classes name of the type token with {@code Impl} suffix
     * added. Generated source code are placed in the correct subdirectory of the specified
     * {@code root} directory and have correct file name. For example, the implementation of the
     * interface {@link java.util.List} would go to {@code $root/java/util/ListImpl.java}
     *
     *
     * @param aClass type token to create implementation for (non-null).
     * @param path root directory.
     * @throws ImplerException when implementation cannot be generated (non-null).
     */
    @Override
    public void implement(Class<?> aClass, Path path) throws ImplerException {
        implementEx(aClass, path);
    }

    /**
     * Changes extension in a path to a file to a different one.
     * <p>
     * Effectively replaces substring from the last occurrence of {@code '.'} character to the end of
     * path's string representation with a specified extension and returns the resulting path.
     *
     * @param path Path to change.
     * @param newExt New extension.
     * @return Path to a file with the same name, but new specified extension.
     */
    private static Path changePathExtension(Path path, String newExt) {
        String fName = path.toString();
        fName = fName.substring(0, fName.lastIndexOf('.'));
        return Path.of(fName + '.' + newExt);
    }

    /**
     * Convert a path to a directory to a string of a path inside a JAR file.
     * <p>
     * Effectively replaces every path separator with a {@code '/'} and places one at the end.
     *
     * @param path Path to a directory.
     * @return Result path in a JAR-compatible format.
     */
    private static String convertDirPathToJarDirPath(Path path) {
        if (path == null) {
            return "";
        }
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < path.getNameCount(); i++) {
            sb.append(path.getName(i)).append('/');
        }
        return sb.toString();
    }

    /**
     * Produces <var>.jar</var> file implementing class or interface specified by provided <var>token</var>.
     * <p>
     * Generated class classes name are the same as classes name of the type token with <var>Impl</var> suffix
     * added.
     *
     * @param aClass type token to create implementation for.
     * @param path target <var>.jar</var> file.
     * @throws ImplerException when implementation cannot be generated.
     */
    @Override
    public void implementJar(Class<?> aClass, Path path) throws ImplerException {
        Path tempDir = Path.of("temp");
        Path pOut = implementEx(aClass, tempDir);

        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        String classpath = ".";
        try {
            CodeSource src = aClass.getProtectionDomain().getCodeSource();
            if (src != null) {
                classpath = Path.of(src.getLocation().toURI()).toString();
            }
        } catch (final java.net.URISyntaxException e) {
            throw new ImplerException("Corrupted class token location");
        }
        int code = compiler.run(null, null, null, pOut.toString(), "-d", tempDir.toString(), "-cp", classpath);
        if (code != 0) {
            throw new ImplerException("Failed to compile implementation class. Sorry");
        }

        Path localPath = changePathExtension(tempDir.relativize(pOut), "class");
        String jarName = convertDirPathToJarDirPath(localPath.getParent()) + localPath.getFileName().toString();

        try (JarOutputStream jarOut = new JarOutputStream(Files.newOutputStream(path))) {
            jarOut.putNextEntry(new ZipEntry(jarName));
            try (InputStream in = Files.newInputStream(tempDir.resolve(localPath))) {
                in.transferTo(jarOut);
            }
            jarOut.closeEntry();
        } catch (IOException exc) {
            throw new ImplerException("Error occurred during writing of result jar file", exc);
        }

        try {
            if (Files.exists(tempDir)) {
                Files.walkFileTree(tempDir, DELETE_VISITOR);
            }
        } catch (IOException exc) {
            System.err.println("Warning: cannot take the trash directory out: " + exc.toString());
        }
    }

    /**
     * Check validity of program command line arguments.
     * <p>
     * Checks that arguments array contains only non-null and exactly a specified number of elements.
     *
     * @param args Arguments array to check.
     * @param length Expected length.
     * @return {@code true} if {@code args.length} equals {@code length} and every element is non-null and
     * {@code false} otherwise.
     */
    private static boolean checkArgsLength(String[] args, int length) {
        return args != null && args.length == length && Arrays.stream(args).noneMatch(Objects::isNull);
    }

    /**
     * Main program function.
     * <p>
     * If number of command line arguments is {@code 2}, then passes them (parsed) to
     * {@link #implement(Class, Path)} method.
     * Otherwise, if number of arguments is {@code 3} and the first one is "-jar", then passes them (parsed) to
     * {@link #implementJar(Class, Path)} method.
     * Otherwise, prints usage in an unspecified format.
     * If an error occurs, prints error message to {@link System#err} in an unspecified format.
     *
     * @param args Command line arguments.
     */
    public static void main(String[] args) {
        try {
            if (checkArgsLength(args, 2)) {
                new Implementor().implement(Class.forName(args[0]), Paths.get(args[1]));
            } else if (checkArgsLength(args, 3) && args[0].equals("-jar")) {
                new Implementor().implementJar(Class.forName(args[1]), Paths.get(args[2]));
            } else {
                System.err.println("Usage: <executing command> <full name of class/interface to implement> " +
                        "<path to output directory>");
            }
        } catch (ClassNotFoundException exc) {
            System.err.println("Cannot find specified class/interface: " + exc.getMessage());
        } catch (InvalidPathException exc) {
            System.err.println("Invalid path: " + exc.getMessage());
        } catch (ImplerException exc) {
            System.err.println("Error while implementing: " + exc.getMessage());
            if (exc.getCause() != null) {
                System.err.println("(Cause: " + exc.getCause().toString() + ")");
            }
        }
    }
}
