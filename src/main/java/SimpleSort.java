package me.penkov.inversions;

public class SimpleSort {
    public static void mergeSort(int[] xs) {
        int[] aux = new int[xs.length];
        mergeSort(xs, aux, 0, xs.length - 1);
    }

    public static void mergeSort(int[] xs, int[]  aux, int left, int right) {
        if (right <= left)  return;

        int m = left + (right - left)/2;

        mergeSort(xs, aux, left, m);
        mergeSort(xs, aux, m+1, right);
        merge(xs, aux, left, right);
    }

    public static void merge(int[] xs, int[] aux, int left, int right) {
        for(int k = left; k <= right; k++) {
            aux[k] = xs[k];
        }

        int m = left + (right - left)/2;

        int i = left;
        int j = m+1;

        for(int k = left; k <= right; k++) {
            if(i > m) xs[k] = aux[j++];
            else if(j > right)  xs[k] = aux[i++];
            else if(aux[j] < aux[i]) xs[k] = aux[j++];
            else xs[k] = aux[i++];
        }
    }

    public static long mergeAndCountInversions(int[] xs, int[] aux, int left, int right) {
        System.arraycopy(xs, left, aux, left, right + 1 - left);

        int m = left + (right - left)/2;

        int i = left;
        int j = m+1;
        long inversions = 0;

        for(int k = left; k <= right; k++) {
            if(i > m) xs[k] = aux[j++];
            else if(j > right) xs[k] = aux[i++];
            else if(aux[j] < aux[i]) {
                xs[k] = aux[j++];
                long curr = (m - i)+1;
                inversions+=curr;
            } else xs[k] = aux[i++];
        }
        return inversions;
    }

    public static int[] merge(int[] xs) {
        int[] aux = new int[xs.length];
        merge(xs, aux, 0, xs.length - 1);
        return xs;
    }

    public static long countInversions(int[] xs) {
        int[] aux = new int[xs.length];
        return sortAndCountInversions(xs, aux, 0, xs.length - 1);
    }

    private static long sortAndCountInversions(int[] xs, int[] aux, int lo, int hi) {
        if (hi <= lo)  return 0;

        int m = lo + (hi - lo)/2;

        long li = sortAndCountInversions(xs, aux, lo, m);
        long ri = sortAndCountInversions(xs, aux, m + 1, hi);
        long si = mergeAndCountInversions(xs, aux, lo, hi);
        return li + ri + si;
    }
}
