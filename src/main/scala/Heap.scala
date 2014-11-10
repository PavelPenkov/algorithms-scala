import scala.collection.mutable.ArrayBuffer
import Utils._

class Heap[a : Ordering] extends Iterable[a] {
  private val order = implicitly[Ordering[a]]
  import order._

  private val heap = ArrayBuffer(default[a])

  def put(x: a) {
    heap append x
    swim()
  }

  override def size = heap.size - 1

  private def swim() {
    var j = size
    while(j > 1 && less(j, j/2)) {
      swap(j, j/2)
      j=j/2
    }
  }

  private def drown(i: Int) {
    var j = i
    var cont = true
    while(2*j <= size && cont) {
      cont = false
      if(2*j + 1 <= size) {
        val k = if(less(2*j, 2*j + 1)) 2*j else 2*j+1
        if(less(k, j)) {
          swap(k, j)
          j = k
          cont = true
        }
      } else {
        if(less(2*j, j)) {
          swap(2*j, j)
          j = 2*j
          cont = true
        }
      }
    }
  }

  def deleteMin = {
    if(size == 0) throw new IllegalStateException() else {
      val min = heap(1)
      swap(1, size)
      heap.remove(size)
      drown(1)
      min
    }
  }
  
  def peek = if(size > 0) heap(1) else throw new IllegalStateException()

  def next() = deleteMin

  def hasNext = size != 0

  private def swap(i: Int, j: Int) = {
    val temp = heap(i)
    heap(i) = heap(j)
    heap(j) = temp
  }

  private def less(i: Int, j: Int): Boolean = heap(i) < heap(j)

  def iterator = ???
}

object Heap {
  def apply[a : Ordering] = new Heap[a]
}
