import me.penkov.utils.Benchmark
import scala.io.Source
import scala.math._

object SccAssignment {
  def graphFromFile(filename: String) = {
    val edges = Source.fromFile("SCC.txt").getLines().map { line =>
      line split "\\s+" match {
        case Array(u, v) => (u.toInt, v.toInt)
      }
    }.toList

    val n = edges.map {case (u,v) => u max v  }.max

    println(s"Graph has $n vertices")

    edges.foldLeft(Digraph(n)) { case (g, (u, v)) => g addEdge (u - 1, v - 1)}
  }

  def main(args: Array[String]): Unit = {
    val (ts, dg) = Benchmark.realtime {
      graphFromFile("SCC.txt")
    }

    println(s"Graph parsed in $ts")

    Benchmark.bm("Reverse graph") {
      println(dg.reversed.n)
    }

    Benchmark.bm("Strongly connected components") {
      val sccs = dg.scc
      println(sccs.sortBy { case (leader, size) => -size }.map(_._2).take(5).mkString(","))
    }
  }
}
