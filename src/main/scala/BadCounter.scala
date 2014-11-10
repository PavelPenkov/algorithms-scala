package me.penkov.inversions

object BadCounter {
  def countInversions(xs: Array[Int]) = {
    val aux = new Array[Int](xs.length)
    sortAndCountInversions(xs, aux, 0, xs.length - 1)
  }

  def mergeAndCountInversions(xs: Array[Int], aux: Array[Int], left: Int, right: Int) = {
    for(k <- left to right) {
      aux(k) = xs(k)
    }

    val m = left + (right - left) /2

    var i = left
    var j = m + 1
    var inv: Long = 0
    var k = left

    while(k <= right) {
      if(i > m) {
        xs(k) = aux(j)
        j+=1
      } else if(j > right) {
        xs(k) = aux(i)
        i+=1
      } else if(aux(j) < aux(i)) {
        xs(k) = aux(j)
        j+=1
        inv+=(m - i) + 1
      } else {
        xs(k) = aux(i)
        i+=1
      }
      k+=1
    }
    inv
  }

  def sortAndCountInversions(xs: Array[Int], aux: Array[Int], left: Int, right: Int) : Long = {
    if(right <= left) 0
    else {
      val m = left + (right - left)/2
      val li = sortAndCountInversions(xs, aux, left, m)
      val ri = sortAndCountInversions(xs, aux, m+1, right)
      val si = mergeAndCountInversions(xs, aux, left, right)
      li + ri + si
    }
  }
}
