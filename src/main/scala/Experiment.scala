import scala.util.Random

object Experiment {
  val rng = new Random()

  val incStream: Stream[Int] = 0 #:: 1 #:: incStream.zip(incStream.tail).map{n => n._2 + rng.nextInt(100)}

  val fibs: Stream[Int] = 0 #:: 1 #:: fibs.zip(fibs.tail).map{n => n._1 + n._2}
}

class Three extends Iterable[Int] {
  var n = 0
  override def iterator = new Iterator[Int] {
    def hasNext = n < 3

    def next = {
      n+=1
      n
    }
  }
}


