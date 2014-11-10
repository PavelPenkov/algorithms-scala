public class Stopwatch {
    private long start;

    public long msElapsed() {
        return System.currentTimeMillis() - start;
    }

    public void restart() {
        start = System.currentTimeMillis();
    }
}
