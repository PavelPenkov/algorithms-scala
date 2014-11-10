case class IndexKeyPair[a](index: Int, key: a)

class IndexedHeap[a : Ordering](maxSize: Int) extends Iterable[IndexKeyPair[a]] {
  import scala.util.control.Breaks._

  private val pq = Array.fill(maxSize + 1) { -1 }
  private val qp = Array.fill(maxSize + 1) { -1 }
  private val keys = Array.fill(maxSize + 1) { Option.empty[a] }
  private var n = 0
  private val order = implicitly[Ordering[a]]

  def changeKey(i: Int, key: a) {
    val oldKey = keys(i).get
    keys(i) = Some(key)
    val j = qp(i)
    if (order.gt(key , oldKey)) drown(j) else swim(j)
  }

  def contains(i: Int) = qp(i) != -1

  def put(i: Int, key: a) {
    if (contains(i)) throw new IllegalArgumentException(i.toString)
    if (i > maxSize-1 || i < 0) throw new IndexOutOfBoundsException(i.toString)
    n+=1
    pq(n) = i
    qp(i) = n
    keys(i) = Some(key)
    swim(n)
  }

  override def size = n

  private def swim(i: Int) {
    var j = i
    while(j > 1 && less(j, j/2)) {
      swap(j, j/2)
      j=j/2
    }
  }

  private def drown(i: Int) = {
    var k = i
    breakable {
      while (2 * k <= n) {
        var j = 2 * k
        if (j < n && less(j+1, j)) { j += 1 }
        if (less(k, j)) break()
        swap(k, j)
        k = j
      }
    }
  }

  def deleteMin = {
    if (n == 0) throw new NoSuchElementException
    val min = peek
    swap(1, n)
    n-=1
    drown(1)
    qp(min.index) = -1
    keys(pq(n+1)) = None
    pq(n+1) = -1
    min
  }

  def peek: IndexKeyPair[a] = if (n == 0) throw new IndexOutOfBoundsException else IndexKeyPair(pq(1), keys(pq(1)).get)

  def peekIndex: Int = peek.index

  def peekKey: a = peek.key

  def get(i: Int): a = keys(i).get

  override def isEmpty = size == 0

  private def swap(i: Int, j: Int) {
    val temp = pq(i)
    pq(i) = pq(j)
    pq(j) = temp

    qp(pq(i)) = i
    qp(pq(j)) = j
  }

  def iterator = {
    val copy = new IndexedHeap[a](maxSize)
    (1 until n) foreach { i => copy.put(pq(i), keys(pq(i)).get) }

    new Iterator[IndexKeyPair[a]] {
      def hasNext = !copy.isEmpty

      def next() = copy.deleteMin
    }
  }

  private def less(i: Int, j: Int) = order.lt(keys(pq(i)).get , keys(pq(j)).get)
}

object IndexedHeap {
  def apply[a : Ordering](maxSize: Int) = new IndexedHeap[a](maxSize)
}
