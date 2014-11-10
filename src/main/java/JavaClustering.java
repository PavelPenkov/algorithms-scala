import java.io.BufferedReader;
import java.io.FileReader;
import java.util.*;

public class JavaClustering {
    public static int toggled(int x, int... bits) {
        for(int i = 0; i < bits.length; i++) {
            x ^= 1 << bits[i];
        }
        return x;
    }

    public static int flipOne(int x, int bit) {
        return x ^ (1 << bit);
    }

    public static ArrayList<Integer> neighbors(int x, int dist, int length) {
        return neighborN(x, 0, dist, length);
    }


    public static ArrayList<Integer> neighbor3(int x, int length) {
        ArrayList<Integer> result = new ArrayList<>();
        for(int i = 0; i < length; i++) {
            for(int j = i; j < length; j++) {
                for(int k = j; k < length; k++) {
                    result.add(toggled(x, i, j, k));
                }
            }
        }
        return result;
    }

    public static ArrayList<Integer> neighborN(int x, int start, int dist, int length) {
        ArrayList<Integer> result = new ArrayList<>();
        if(dist == 0) {
            result.add(x);
        } else {
            for(int i = start; i < length; i++) {
                for(Integer neighbor: neighborN(toggled(x, i), i+1, dist-1, length)) {
                    result.add(neighbor);
                }
            }
        }
        return result;
    }

    public static class Params {
        public final int _x;
        public final int _start;
        public final int _dist;

        public Params(int x, int start, int dist) {
            _x = x; _start = start; _dist = dist;
        }
    }

    public static ArrayList<Integer> neighborLoop(int x, int start, int dist, int length) {
        ArrayList<Integer> result = new ArrayList<>();
        Stack<Params> stack = new Stack<>();
        stack.push(new Params(x, start, dist));

        while(!stack.isEmpty()) {
            Params params = stack.pop();
            if(params._dist == 0) {
                result.add(params._x);
            } else {
                for(int i = params._start; i < length; i++) {
                    stack.push(new Params(flipOne(x, i), i+1, params._dist-1));
                }
            }
        }

        return result;
    }

    public static <T> ArrayList flatten(ArrayList<ArrayList<T>> xss) {
        ArrayList<T> result = new ArrayList<>();
        for(ArrayList<T> xs: xss) {
            for(T x: xs) {
                result.add(x);
            }
        }
        return result;
    }

    public static int fromBitString(String s) {
        String[] bits = s.split("\\s+");
        int x = 0;
        // System.out.println("length=" + bits.length);
        for(int i = 0; i < bits.length; i++) {
            int digit = bits.length - i - 1;
            if(bits[i].equals("1")) x+= (1 << (digit));
            // System.out.format("Bit %d is %s x is %d\n", digit, bits[i], x);
        }
        return x;
    }

    public static int clusters(int[] vertices, int bits, int spacing) {
        HashMap<Integer, ArrayList<Integer>> coordsToVertex = new HashMap<>();
        for(int i = 0; i < vertices.length; i++) {
            if(!coordsToVertex.containsKey(vertices[i])) {
                coordsToVertex.put(vertices[i], new ArrayList<Integer>());
            }
            coordsToVertex.get(vertices[i]).add(i);
        }

        JavaUf uf = new JavaUf(vertices.length);
        for(int j = 0; j < vertices.length; j++) {
            for(int spc = 1; spc < spacing; spc++) {
                for (Integer neighbor : neighbors(vertices[j], spc, bits)) {
                    List<Integer> nvs = coordsToVertex.get(neighbor);
                    if (nvs != null) {
                        if (nvs.size() > 1) System.out.printf("%d neighbors for %d\n", nvs.size(), j);
                        for (Integer v : nvs) {
                            if(v != j) {
                                uf.union(j, v);
                            }
                        }
                    }
                }
            }
        }

        return uf.components;
    }

    public static void main(String[] args) throws java.io.IOException {
        /*
        BufferedReader in = new BufferedReader(new FileReader("clustering_big.txt"));
        int i = 0;
        int size = 500;
        int vertices[] = new int[size];
        String s;
        in.readLine();
        while((s = in.readLine()) != null && i < size) {
            int coords = fromBitString(s);
            vertices[i] = coords;
            i++;
        }

        int clusters = clusters(vertices, 24, 3);

        System.out.println("Total clusters " + clusters);
        */

        BufferedReader in = new BufferedReader(new FileReader("clustering_big.txt"));
        int i = 0;
        int size = Integer.parseInt(in.readLine().split("\\s")[0]);
        int vertices[] = new int[size];
        String s;
        while((s = in.readLine()) != null) {
            int coords = fromBitString(s);
            vertices[i] = coords;
            i++;
        }

        int clusters = clusters(vertices, 24, 3);

        System.out.println("Total clusters " + clusters);
    }
}
