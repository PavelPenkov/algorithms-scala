import me.penkov.utils.Benchmark
import scala.io.Source
import scala.math._

object SccAssignment {
  def main(args: Array[String]): Unit = {
    val (ts, dg) = Benchmark.realtime {
      Digraph.fromFile("SCC.txt")
    }

    println(s"Graph parsed in $ts")
    println(s"Vertices ${dg.n} edges ${dg.M}")

    val (ts1, ids) = Benchmark.realtime {
      dg.sccTarjan
    }
    println(s"Tarjan SCC in $ts1")

    val (ts2, ids3) = Benchmark.realtime {
      dg.sccKosaraju
    }
    println(s"Kosaraju SCC $ts2")

    val (ts3, ids2) = Benchmark.realtime {
      dg.sccPathBased
    }
    println(s"Gabow SCC in $ts3")


    val sccs = ids.zipWithIndex.groupBy(_._1).mapValues(_.length)
    println(sccs.toList.sortBy(_._2)(Ordering[Int].reverse).take(5).mkString(","))
  }
}
