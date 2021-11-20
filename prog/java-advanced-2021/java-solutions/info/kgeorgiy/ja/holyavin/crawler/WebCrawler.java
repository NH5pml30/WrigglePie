package info.kgeorgiy.ja.holyavin.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.*;
import java.util.function.Function;
import java.util.Optional;
import java.util.stream.Collectors;

public class WebCrawler implements AdvancedCrawler {
    private final int perHost;
    private final Downloader downloader;
    private final Map<String, Semaphore> hostSemaphores = new HashMap<>();
    private final ExecutorService downloaderService, extractorService;
    private final Function<String, Semaphore> semaphoreFactory;

    public WebCrawler(Downloader downloader, int downloaders, int extractors, int perHost) {
        this.downloader = downloader;
        downloaderService = Executors.newFixedThreadPool(downloaders);
        extractorService = Executors.newFixedThreadPool(extractors);
        this.perHost = perHost;
        semaphoreFactory = urlKey -> new Semaphore(perHost);
    }

    private interface IOCallable<T> {
        T call() throws IOException;
    }
    private static final Optional<IOException> OK_MARKER = Optional.of(new IOException());
    private class ResultBuffer {
        private final ConcurrentMap<String, String> hosts;
        private final ConcurrentLinkedQueue<DownloadTask> nextDownloads = new ConcurrentLinkedQueue<>();
        private final ConcurrentMap<String, Optional<IOException>> pages = new ConcurrentHashMap<>();
        // page absent: not downloaded
        // page -> <empty>: download requested
        // page -> OK_MARKER downloaded ok
        // page -> != OK_MARKER: the error

        final Phaser phaser = new Phaser(1);

        private ResultBuffer(List<String> hosts) {
            if (hosts != null) {
                this.hosts = hosts.stream().collect(Collectors.toConcurrentMap(Function.identity(), Function.identity(),
                        (x, y) -> y, ConcurrentHashMap::new));
            } else {
                this.hosts = null;
            }
        }

        void addError(String url, IOException exc) {
            pages.put(url, Optional.of(exc));
        }

        private <T> T safeWrapper(String url, IOCallable<T> action) {
            T result = null;
            try {
                result = action.call();
            } catch (IOException exc) {
                addError(url, exc);
            }
            return result;
        }

        List<String> extractLinks(String url, Document doc) {
            return safeWrapper(url, doc::extractLinks);
        }

        String getHost(String url) {
            return safeWrapper(url, () -> URLUtils.getHost(url));
        }

        boolean requestDownload(String url) {
            String host = getHost(url);
            if (hosts != null && (host == null || !hosts.containsKey(host))) {
                return false;
            }
            if (pages.putIfAbsent(url, Optional.empty()) != null) {
                return false;
            }
            nextDownloads.add(new DownloadTask(url, this));
            return true;
        }

        Document download(String url) {
            Document doc = null;
            pages.put(url, OK_MARKER);
            try {
                doc = downloader.download(url);
            } catch (IOException exc) {
                addError(url, exc);
            }
            return doc;
        }

        Result extractData() {
            var map =
                    pages.entrySet().stream().collect(Collectors.partitioningBy(entry ->
                            entry.getValue().orElse(null) == OK_MARKER.get()));
            var downloaded =
                    map.get(Boolean.TRUE).stream().map(Map.Entry::getKey).collect(Collectors.toUnmodifiableList());
            var errors = map.get(Boolean.FALSE).stream().filter(e -> e.getValue().isPresent()).
                    collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().get()));
            return new Result(downloaded, errors);
        }
    }

    private class ExtractTask implements Runnable {
        final String url;
        final Document document;
        final ResultBuffer result;

        private ExtractTask(String url, Document document, ResultBuffer result) {
            this.url = url;
            this.document = document;
            this.result = result;
        }

        @Override
        public void run() {
            List<String> links = result.extractLinks(url, document);
            if (links != null) {
                links.forEach(result::requestDownload);
            }
            result.phaser.arriveAndDeregister();
        }
    }

    private class DownloadTask implements Runnable {
        final String url, host;
        final ResultBuffer result;

        DownloadTask(String url, ResultBuffer result) {
            this.url = url;
            this.host = result.getHost(url);
            this.result = result;
        }

        @Override
        public void run() {
            try {
                if (host != null) {
                    Semaphore semaphore;
                    synchronized (hostSemaphores) {
                        semaphore = hostSemaphores.computeIfAbsent(host, semaphoreFactory);
                    }
                    semaphore.acquire();
                    Document d = result.download(url);
                    semaphore.release();
                    if (d != null) {
                        result.phaser.register();
                        extractorService.submit(new ExtractTask(url, d, result));
                    }
                }
            } catch (InterruptedException ignored) {
                Thread.currentThread().interrupt();
            } finally {
                result.phaser.arriveAndDeregister();
            }
        }
    }

    @Override
    public Result download(String url, int depth) {
        return download(url, depth, null);
    }

    private void nextLayer(ResultBuffer result) {
        var saved = result.nextDownloads.toArray(new DownloadTask[0]);
        result.nextDownloads.clear();
        result.phaser.bulkRegister(saved.length);
        for (var task : saved) {
            downloaderService.submit(task);
        }
        result.phaser.arriveAndAwaitAdvance();
    }

    @Override
    public Result download(String url, int depth, List<String> hosts) {
        ResultBuffer result = new ResultBuffer(hosts);
        result.requestDownload(url);
        for (int i = 0; i < depth; i++) {
            nextLayer(result);
        }
        return result.extractData();
    }

    private static boolean awaitTermination(ExecutorService service) {
        boolean interrupted = false;
        while (true) {
            try {
                if (service.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS)) {
                    break;
                }
            } catch (InterruptedException e) {
                service.shutdownNow();
                interrupted = true;
            }
        }
        return interrupted;
    }

    @Override
    public void close() {
        downloaderService.shutdownNow();
        extractorService.shutdownNow();
        if (awaitTermination(downloaderService) | awaitTermination(extractorService)) {
            Thread.currentThread().interrupt();
        }
    }

    public static void main(String[] args) {
        String usage = "WebCrawler url [depth=2 [downloaders=8 [extractors=8 [perHost=8]]]]";
        if (args.length < 1) {
            System.out.println(usage);
            return;
        }

        Downloader downloader;
        try {
            downloader = (Downloader) Class.forName("info.kgeorgiy.java.advanced.crawler.CachingDownloader")
                    .getConstructor().newInstance();
        } catch (Exception e) {
            System.out.println("Cannot create appropriate downloader: " + e);
            return;
        }

        try (WebCrawler crawler = new WebCrawler(downloader,
                args.length >= 3 ? Integer.parseInt(args[2]) : 8,
                args.length >= 4 ? Integer.parseInt(args[3]) : 8,
                args.length >= 5 ? Integer.parseInt(args[4]) : 8)) {
            String url = args[0];
            int depth = args.length >= 2 ? Integer.parseInt(args[1]) : 2;

            Result result = crawler.download(url, depth);
            System.out.println("Successfully loaded:");
            for (String downloaded : result.getDownloaded()) {
                System.out.println(downloaded);
            }
            if (!result.getErrors().isEmpty()) {
                System.out.println("---------------------------");
                System.out.println("Encountered errors:");
                for (var entry : result.getErrors().entrySet()) {
                    System.out.println(entry.getKey() + ": " + entry.getValue().toString());
                }
            }
        } catch (NumberFormatException exc) {
            System.out.println("Invalid integer argument format: " + exc.getMessage());
        }
    }
}
