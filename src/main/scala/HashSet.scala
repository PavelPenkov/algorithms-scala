class HashSet[A] extends scala.collection.mutable.Set[A] {
  // Not a true sieve
  private def sieve(xs: Stream[Int]): Stream[Int] = xs.head #:: sieve(xs.tail).filter(_ % xs.head != 0)

  private val primes = (2 #:: sieve(Stream.from(3, 2))).iterator

  private var n = primes.next()

  private var buckets = Array.fill(n) { List.empty[A] }

  private var sz = 0

  def fillFactor = sz.toDouble / n

  override def +=(elem: A) = {
    if (fillFactor > 0.5) rehash()
    val i = bucketFor(elem)
    buckets(i) = elem::buckets(i)
    sz+=1
    this
  }

  private def bucketFor(x: A) = x.hashCode() % n

  private def rehash() {
    n = primes.next()
    val newBuckets = Array.fill(n) { List.empty[A] }
    buckets.toList.flatten.foreach { x =>
      val i = bucketFor(x)
      newBuckets(i) = x::newBuckets(i)
    }

    buckets = newBuckets
  }

  override def -=(elem: A) = {
    val i = bucketFor(elem)
    buckets(i) = buckets(i) filterNot (_ == elem)
    this
  }

  override def contains(elem: A) = buckets(bucketFor(elem)).contains(elem)

  override def iterator: Iterator[A] = buckets.toList.flatten.iterator
}

object HashSet {
  def apply[A] = new HashSet[A]
}
