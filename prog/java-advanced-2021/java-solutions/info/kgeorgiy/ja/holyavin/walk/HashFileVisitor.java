package info.kgeorgiy.ja.holyavin.walk;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;

class HashFileVisitor implements FileVisitor<Path> {
    private final Writer outFile;

    // Fixing unpushed note
    private static class MyBufferedInputStream implements AutoCloseable {
        private final byte[] buf = new byte[8 * 1024];
        private int size = 0, pos = 0;
        private final InputStream is;

        private MyBufferedInputStream(InputStream is) {
            this.is = is;
        }

        private int read() throws IOException {
            if (pos == size) {
                if ((size = is.read(buf)) < 0) {
                    return -1;
                }
                pos = 0;
            }
            return Byte.toUnsignedInt(buf[pos++]);
        }

        @Override
        public void close() throws IOException {
            is.close();
        }
    };

    public HashFileVisitor(Writer outFile) throws IOException {
        this.outFile = outFile;
    }

    private static long countPJWHash64(MyBufferedInputStream is) throws IOException {
        int ch;
        long res = 0;
        while ((ch = is.read()) >= 0) {
            res = (res << 8) + ch;
            long highBits = res & (0xFFL << (64 - 8));
            res ^= highBits >> (64 * 3 / 4);
            res &= ~highBits;
        }
        return res;
    }

    final long ERROR_HASH = 0;

    void writeHash(String fileName, long hash) throws IOException {
        outFile.write(String.format("%1$016x", hash) + " " + fileName + System.lineSeparator());
    }

    void writeErrorHash(String fileName) throws IOException {
        writeHash(fileName, ERROR_HASH);
    }

    @Override
    public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
        long hash = ERROR_HASH;
        try (MyBufferedInputStream br = new MyBufferedInputStream(Files.newInputStream(file))) {
            hash = countPJWHash64(br);
        } catch (IOException exc) {
            System.err.println("Visit of file '" + file + "' completed prematurely: " + exc.getMessage());
            return FileVisitResult.CONTINUE;
        } finally {
            writeHash(file.toString(), hash);
        }
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
        System.err.println("File '" + file + "' could not be visited: " + exc.getMessage());
        writeErrorHash(file.toString());
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult postVisitDirectory(Path dir, IOException exc) {
        if (exc != null) {
            System.err.println("Iteration of the directory '" + dir + "' completed prematurely: " + exc.getMessage());
        }
        return FileVisitResult.CONTINUE;
    }
}
