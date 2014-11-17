import scala.io._

object Clustering extends App {
  def hammingDistance(x: Int, y: Int) = {
    var dst = 0
    var diff = x ^ y
    while(diff != 0) {
      dst+=1
      diff &= diff -1
    }
    dst
  }

  val src = Source.fromFile("clustering_big.txt").getLines()

  val n = src.next().toInt

  val g = new WeightedGraph(n)

  src foreach { s =>
    s split "\\s" map (_.toInt) match {
      case Array(u, v, w) => g.addEdge(u - 1, v - 1, w)
    }
  }

  println(s"Spacing is ${g.clusterSpacing(4)}")
}
