public class JavaUf {
    private final int[] parent;
    private final int[] rank;
    public int components;

    public JavaUf(int n) {
        parent = new int[n];
        rank = new int[n];
        for(int i = 0; i < n; i++) {
            parent[i] = i;
        }
        components = n;
    }

    public boolean connected(int x, int y) {
        return find(x) == find(y);
    }

    public int find(int x) {
        if(x != parent[x]) {
            parent[x] = find(parent[x]);
        }
        return parent[x];
    }

    public void union(int x, int y) {
        int px = find(x);
        int py = find(y);
        if(px != py) {
            link(px, py);
            components--;
        }
    }

    public void link(int x, int y) {
        if(rank[x] > rank[y]) {
            parent[y] = x;
        } else {
            parent[x] = y;
        }
        if(rank[x] == rank[y]) {
            rank[y]+=1;
        }
    }
}
