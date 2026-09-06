import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

/** Runs an isolated Scala snapshot with an independent shutdown timeout. */
class RuntimeBenchmark {
    public static void main(String[] args) throws ReflectiveOperationException {
        int seconds = Integer.getInteger("bench.seconds", 180);
        var watchdog = Executors.newSingleThreadScheduledExecutor(r -> {
            Thread t = new Thread(r, "benchmark-watchdog");
            t.setDaemon(true);
            return t;
        });
        watchdog.schedule(() -> {
            System.out.println("[watchdog] JVM timeout, including setup/shutdown");
            System.exit(124);
        }, seconds + 60L, TimeUnit.SECONDS);
        long start = System.nanoTime();
        // Historical snapshots have four constructor arguments; current models add autoRun.
        Class<?> model = scalation.simulation.process.model.CalRoute101_3.class;
        try {
            model.getConstructor(String.class, int.class, boolean.class, double.class, boolean.class)
                 .newInstance("CalRoute101_3", 1, Boolean.getBoolean("bench.animate"), 500.0, true);
        } catch (NoSuchMethodException oldSnapshot) {
            model.getConstructor(String.class, int.class, boolean.class, double.class)
                 .newInstance("CalRoute101_3", 1, Boolean.getBoolean("bench.animate"), 500.0);
        }
        System.out.println("[jvm-complete] seconds=" + (System.nanoTime() - start) / 1e9);
        watchdog.shutdownNow();
    }
}
