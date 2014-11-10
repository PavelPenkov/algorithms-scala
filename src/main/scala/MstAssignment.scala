import scala.io._
object MstAssignment extends App {
  val src = Source.fromFile("edges.txt").getLines()
  val n = src.next().split("\\s")(0).toInt
  val wg = new WeightedGraph(n)

  src foreach { line => line split "\\s" match { case Array(u, v, w) => wg.addEdge(u.toInt - 1, v.toInt - 1, w.toInt) } }

  println(s"MST weight (Prim): ${wg.mstWeightPrim}")
  println(s"MST weight (Kruskal): ${wg.mstWeightKruskal}")
}
