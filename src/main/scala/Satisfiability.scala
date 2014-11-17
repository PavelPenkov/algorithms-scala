import java.util.BitSet
import math._
import scala.io._
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.control.Breaks._

sealed trait BooleanVar {
  def index: Int
}

case class Self(i: Int) extends BooleanVar {
  val index = i
}
case class Not(i: Int) extends BooleanVar {
  val index = i
}

case class BooleanExpr(x: BooleanVar, y: BooleanVar) {
  def apply(vars: BitSet): Boolean = f(vars)

  val f = (x, y) match {
    case (Self(i), Self(j)) => { (set: BitSet) => set.get(i) || set.get(j) }
    case (Self(i), Not(j)) => { (set: BitSet) => set.get(i) || !set.get(j) }
    case (Not(i), Self(j)) => { (set: BitSet) => !set.get(i) || set.get(j) }
    case (Not(i), Not(j)) => { (set: BitSet) => !set.get(i) || !set.get(j)}
  }
}


class Satisfiability(val instance: List[BooleanExpr]) {
  val n = instance.length
  val rng = new scala.util.Random()

  def solveRandomized(implicit ctx: ExecutionContext): Boolean = {
    var i = 0
    var result = false
    while(!result && i < (math.log(n) / math.log(2)).toInt + 1) {
      val threads = 4
      val futures = (1 to threads).map { _ =>
        val values = new BitSet(n)
        for (j <- 0 until n) {
          values.set(j, rng.nextBoolean())
        }
        solveRandomizedIter(values)
      }
      val loopResult = Future.fold(futures)(false)((acc, r) => acc || r)
      result = Await.result(loopResult, Duration.Inf)
    }
    result
  }

  private def solveRandomizedIter(initial: BitSet)(implicit ctx: ExecutionContext) : Future[Boolean] = Future {
      val rng = new scala.util.Random()
      var result = false
        var k = 0l
        while (k < 2 * n.toLong * n.toLong && !result) {
          instance.find { clause => !clause(initial)} match {
            case Some(BooleanExpr(l, r)) => if (rng.nextBoolean()) initial.flip(l.index) else initial.flip(r.index)
            case None => { result = true; }
          }
          k += 1
        }
      result
    }

  def solveScc(sccProc: Digraph => Seq[Int]): Boolean = {
    val dg = Digraph(2 * n)
    instance foreach { clause => addEdge(clause, dg) }
    val scc = sccProc(dg)
    for(i <- 0 until n) {
      if(scc(i) == scc(i+n)) return false
    }
    true
  }

  def solveSccPathBased: Boolean = {
    val dg = Digraph(2 * n)
    instance foreach { clause => addEdge(clause, dg) }
    val scc = dg.sccPathBased
    for(i <- 0 until n) {
      if(scc(i) == scc(i+n)) return false
    }
    true
  }

  def addEdge(expr: BooleanExpr, g: Digraph): Unit = {
    // x => x, !x => x + n
    expr match {
      case BooleanExpr(Self(u), Self(v)) => { // !u -> v, !v -> u
        g.addEdge(u+n, v)
        g.addEdge(v+n, u)
      }
      case BooleanExpr(Self(u), Not(v)) => { // !u -> !v, v -> u
        g.addEdge(u+n, v+n)
        g.addEdge(v, u)
      }
      case BooleanExpr(Not(u), Self(v)) => { // u -> v, !v -> !u
        g.addEdge(u, v)
        g.addEdge(v+n, u+n)
      }
      case BooleanExpr(Not(u), Not(v)) => { // u -> !v, v -> !u
        g.addEdge(u, v+n)
        g.addEdge(v, u+n)
      }
    }
  }
}


object Satisfiability {
  def fromFile(filename: String) = {
    val src = Source.fromFile(filename).getLines()

    src.next()

    val clauses = src.map { line =>
      line.split("\\s+").map(_.toInt) match {
        case Array(x, y) => {
          val left: BooleanVar = if (x > 0) Self(x-1) else Not(abs(x)-1)
          val right: BooleanVar = if (y > 0) Self(y-1) else Not(abs(y)-1)
          BooleanExpr(left, right)
        }
      }
    }.toList

    new Satisfiability(clauses)
  }
}
