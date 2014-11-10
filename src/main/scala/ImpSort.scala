import scala.util.Random

object ImpSort {
  val rng = new Random()
  def swap[a](xs: Array[a], i: Int, j: Int):Unit = {
    val temp = xs(i)
    xs(i) = xs(j)
    xs(j) = temp
  }


  def partition[a <% Ordered[a]](xs: Array[a], l: Int, r: Int) = {
    val pivot = xs(l)
    var flt = l + 1
    for(i <- (l+1).to(r)) {
      if(xs(i) < pivot) {
        swap(xs, flt, i)
        flt+=1
      }
    }
    swap(xs, l, flt - 1)
    flt - 1
  }


  def qsort[a <% Ordered[a]](xs: Array[a])(pivotIndex: (Int, Int) => Int): Unit = qsort(xs, 0, xs.length - 1)(pivotIndex)

  def qsortHead[a <% Ordered[a]](xs: Array[a]) = qsort(xs){(l, r) => l}

  def qsortLast[a <% Ordered[a]](xs: Array[a]) = qsort(xs){(l, r) => r}

  def qsortRandom[a <% Ordered[a]](xs: Array[a]) = qsort(xs){(l, r) => l + rng.nextInt(r-l)}

  def qsortAndCount[a <% Ordered[a]](xs: Array[a])(pivotIndex: (Int, Int) => Int): Int = qsortAndCount(xs, 0, xs.length - 1)(pivotIndex)

  def qsortAndCountHead[a <% Ordered[a]](xs: Array[a]) = qsortAndCount(xs){(l, r) => l}

  def qsortAndCountLast[a <% Ordered[a]](xs: Array[a]) = qsortAndCount(xs){(l, r) => r}

  def qsortAndCountRandom[a <% Ordered[a]](xs: Array[a]) = qsortAndCount(xs){(l, r) => l + rng.nextInt(r-l)}

  def qsortAndCountMedian[a <% Ordered[a]](xs: Array[a]): Int = qsortAndCountMedian(xs, 0, xs.length - 1)

  def qsortAndCountMedian[a <% Ordered[a]](xs: Array[a], l: Int, r: Int): Int = {
    val length = r - l // in fact length - 1
    if(length > 0) {
      val pi = medianOfThreeIndex(xs, l, r)
      swap(xs, l, pi)
      val pos = partition(xs, l, r)
      val lc = qsortAndCountMedian(xs, l, pos - 1)
      val rc = qsortAndCountMedian(xs, pos+1, r)
      lc + rc + length
    } else 0
  }

  private def qsortAndCount[a <% Ordered[a]](xs: Array[a], l: Int, r: Int)(pivotIndex: (Int, Int) => Int): Int = {
    val length = r - l // in fact length - 1
    if(length > 0) {
      val pi = pivotIndex(l, r)
      swap(xs, l, pi)
      val pos = partition(xs, l, r)
      val lc = qsortAndCount(xs, l, pos - 1)(pivotIndex)
      val rc = qsortAndCount(xs, pos+1, r)(pivotIndex)
      lc + rc + length
    } else 0
  }

  private def qsort[a <% Ordered[a]](xs: Array[a], l: Int, r: Int)(pivotIndex : (Int, Int) => Int): Unit = {
    if(r - l >= 1) {
      val pi = pivotIndex(l, r)
      swap(xs, l, pi)
      val pos = partition(xs, l, r)
      qsort(xs, l, pos - 1)(pivotIndex)
      qsort(xs, pos+1, r)(pivotIndex)
    }
  }

  def medianOfThreeIndex[a <% Ordered[a]](xs: Array[a], l: Int, r: Int) = { // Just don't want head be bothered with types
    val mid = (r + l) / 2
    val a = xs(l); val b = xs(r); val c = xs(mid)
    if((a < b && b < c) || (c < b && b < a)) r
    else if((a < c && c < b) || (b < c && c < a)) mid else l
  }
}
