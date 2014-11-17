import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class JavaClustering {
    private final int[] coords;
    private final int length;
    private final BitString bs;

    public JavaClustering(int[] coords, int length) {
        this.length = length;
        this.coords = coords;
        bs = new BitString(length);
    }

    public static int fromBitString(String s) {
        return Integer.parseInt(s.replaceAll("\\s+", ""), 2);
    }

    public static JavaClustering fromFile(String filename) throws IOException {
        BufferedReader in = null;
        try {
            in = new BufferedReader(new FileReader(filename));
            int i = 0;
            String[] split = in.readLine().split("\\s");
            int size = Integer.parseInt(split[0]);
            int length = Integer.parseInt(split[1]);
            int[] coords = new int[size];
            String s;
            while((s = in.readLine()) != null) {
                int coord = fromBitString(s);
                coords[i] = coord;
                i++;
            }
            return new JavaClustering(coords, length);
        } finally {
            if (in != null) in.close();
        }
    }

    public int clusters(int spacing) {
        HashMap<Integer, ArrayList<Integer>> coordsToVertex = new HashMap<>();
        for(int i = 0; i < coords.length; i++) {
            if(!coordsToVertex.containsKey(coords[i])) {
                coordsToVertex.put(coords[i], new ArrayList<Integer>());
            }
            coordsToVertex.get(coords[i]).add(i);
        }

        JavaUf uf = new JavaUf(coords.length);
        for(int j = 0; j < coords.length; j++) {
            for(int spc = 0; spc < spacing; spc++) {
                for (Integer neighbor : bs.neighbors(coords[j], spc)) {
                    List<Integer> nvs = coordsToVertex.get(neighbor);
                    if (nvs != null) {
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
        JavaClustering instance = JavaClustering.fromFile("clustering_big.txt");

        System.out.println(instance.clusters(3));
    }
}
