class MedianMaintainer[a : Ordering] {
  private val upper = Heap[a]
  private val lower = Heap[a](implicitly[Ordering[a]].reverse)

  val order = implicitly[Ordering[a]]

  def percentile: a = lower.peek

  def put(x: a) = {
    if (lower.size == 0) {
      lower put x
    } else {
      if (upper.size == lower.size) {
        if (order.lt(x , percentile)) lower put x
        else {
          upper put x
          lower put upper.deleteMin
        }
      } else {
        if (order.gt(x , percentile)) upper put x
        else {
          lower put x
          upper put lower.deleteMin
        }
      }
    }
    this
  }
}

object MedianMaintainer {
  def apply[a : Ordering] = new MedianMaintainer[a]
}
