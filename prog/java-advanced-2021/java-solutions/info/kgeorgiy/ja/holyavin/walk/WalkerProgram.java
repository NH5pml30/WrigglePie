package info.kgeorgiy.ja.holyavin.walk;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.io.Writer;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.Objects;
import java.util.stream.Stream;

class WalkerProgram {
    private final Path pIn, pOut;
    private final int maxDepth;
    private HashFileVisitor vis;

    private WalkerProgram(Path pIn, Path pOut, int maxDepth) {
        this.pIn = pIn;
        this.pOut = pOut;
        this.maxDepth = maxDepth;
    }

    private void handleLine(String pathStr) {
        Path path = null;
        try {
            path = Paths.get(pathStr);
        } catch (InvalidPathException exc) {
            System.err.println("Invalid input file format: " + exc.getMessage());
        }

        try {
            if (path != null) {
                Files.walkFileTree(Paths.get(pathStr), EnumSet.noneOf(FileVisitOption.class), maxDepth, vis);
            } else {
                vis.writeErrorHash(pathStr);
            }
        } catch (IOException exc) {
            System.err.println("Could not write result to output file: " + exc.getMessage());
        }
    }

    private void run() {
        try {
            Path containingDirPath = pOut.getParent();
            if (containingDirPath != null) {
                Files.createDirectories(containingDirPath);
            }
        } catch (IOException exc) {
            System.err.println("Could not create output file directory: " + exc.getMessage());
            return;
        }

        try (Stream<String> in = Files.lines(pIn);
             Writer out = Files.newBufferedWriter(pOut)) {
            vis = new HashFileVisitor(out);
            in.forEachOrdered(this::handleLine);
        } catch (UncheckedIOException exc) {
            System.err.println("Error while reading input file: " + exc.getMessage());
        } catch (IOException exc) {
            System.err.println("Invalid program path argument (cannot open io file): " + exc.getMessage());
        }
    }

    public static void executeProgram(String[] args, int maxDepth) {
        final String usage = "Usage: java Walk <input file> <output file>";
        if (args == null || args.length != 2 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.out.println(usage);
            return;
        }
        Path pIn, pOut;
        try {
            pIn = Paths.get(args[0]);
            pOut = Paths.get(args[1]);
        } catch (InvalidPathException exc) {
            System.err.println("Invalid program path argument (cannot parse io file path): " + exc.getMessage());
            System.out.println(usage);
            return;
        }
        new WalkerProgram(pIn, pOut, maxDepth).run();
    }
}
