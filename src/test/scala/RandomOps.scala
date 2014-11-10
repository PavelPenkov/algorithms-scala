import scala.collection.generic.CanBuildFrom
import scala.util.Random

object RandomOps {
  val rng = new Random()

  def randomInts(n: Int): List[Int] = rng.shuffle(1 to n).toList

  def randomIntsZero(n: Int) = rng.shuffle(0 to (n-1)).toList

  def shuffle[T](xs: TraversableOnce[T]) = rng.shuffle(xs)
}
