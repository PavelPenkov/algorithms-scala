import java.util.*;

public class BitString {
    private final int length;

    public BitString(int length) {
        if (length < 1 || length > 32) throw new IllegalArgumentException("length");
        this.length = length;
    }

    public int hammingDistance(int x, int y) {
        int dist = 0;
        int val = x ^ y;
        while(val != 0) {
            dist++;
            val &= val - 1;
        }
        return dist;
    }

    public Iterable<Integer> neighbors(int x, int dist) {
        return neighborsN(x, 0, dist);

    }

    private ArrayList<Integer> neighborsN(int x, int start, int dist) {
        ArrayList<Integer> result = new ArrayList<>();
        if(dist == 0) {
            result.add(x);
        } else {
            for(int i = start; i < length; i++) {
                for(Integer neighbor: neighborsN(toggled(x, i), i + 1, dist - 1)) {
                    result.add(neighbor);
                }
            }
        }
        return result;
    }

    public static int toggled(int x, int... bits) {
        for(int i = 0; i < bits.length; i++) {
            x ^= 1 << bits[i];
        }
        return x;
    }
}
