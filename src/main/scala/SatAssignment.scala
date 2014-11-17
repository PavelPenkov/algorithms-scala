import me.penkov.utils.Benchmark
import me.penkov.utils.Benchmark._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

object SatAssignment extends App {
  /*
  val (ts, sat) = realtime { Satisfiability.fromFile(s"2sat1.txt") }
  println(s"File 1 read in $ts")
  println(s"${sat.n} clauses")

  val (ts1, result) = realtime { sat.solveScc }
  println(s"Solved in $ts1, result $result")
  */



  for (i <- 1 to 6) {
    val (ts, sat) = realtime {
      Satisfiability.fromFile(s"2sat$i.txt")
    }
    println(s"File $i read in $ts")
    println(s"${sat.n} clauses")

    Benchmark.bm("Tarjan") {
      sat.solveScc(dg => dg.sccTarjan)
    }

    Benchmark.bm("Gabow") {
      sat.solveScc(dg => dg.sccPathBased)
    }

    Benchmark.bm("Kosaraju") {
      sat.solveScc(dg => dg.sccKosaraju)
    }
  }
}

