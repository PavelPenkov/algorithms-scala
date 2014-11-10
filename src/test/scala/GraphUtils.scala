import scala.math._

object GraphUtils {
  def circle(n: Int) = {
    val g = Graph(n)
    (0 to n-1) foreach { i => g.addEdge(i, (i+1) % n) }
    g
  }

  def line(n: Int) = {
    val g = Graph(n)
    (0 until n-1) foreach { i => g.addEdge(i, i+1)}
    g
  }

  def fullTree(rank: Int) = {
    val n = pow(2, rank).toInt - 1
    val g = Graph(n)
    val last = pow(2, rank - 1).toInt - 1
    (0 until last) foreach { i =>
      g.addEdge(i, 2 * i + 1)
      g.addEdge(i, 2 * i + 2)
    }
    g
  }

}
