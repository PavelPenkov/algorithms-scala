import scala.io._

object SSSPAssignment extends App {
  val lines = Source.fromFile("dijkstraData.txt").getLines().toList
  val g = WeightedDigraph(lines.length)
  lines foreach { line =>
    line split "\\s+" match {
      case Array(src, edges@_*) =>
        edges foreach { edge =>
          edge split "," match {
            case Array(dst, weight) => {
              g.addEdge(src.toInt - 1, dst.toInt - 1, weight.toInt)
            }
          }
        }
    }
  }


  val vertices = List(7,37,59,82,99,115,133,165,188,197)
  val dsts = g.shortestDistances(0).toIndexedSeq
  val answer = vertices map { v => dsts(v-1).toInt }
  println(answer mkString ",")
}
