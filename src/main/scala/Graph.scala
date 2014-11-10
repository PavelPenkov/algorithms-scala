import scala.io.Source

object Graph {
  def apply(n: Int) = new Graph(n, Array.fill(n)(List.empty[Int]))

  def apply(s: Source) = {
    val rows = s.getLines() map { line =>
      line.split("\\s+").toList.tail.map(_.toInt - 1)
    }

    val adj = rows.toArray

    new Graph(adj.length, adj)
  }
}

class Graph private (val n: Int, val adj: Array[List[Int]]) {
  import Utils._

  def addEdge(u: Int, v: Int) {
    adj(u) = v::adj(u)
    adj(v) = u::adj(v)
  }

  def edges = for((edges, v) <- adj.zipWithIndex; tail <- edges) yield (v, tail)

  def edgeCount = adj.map(_.length).sum / 2

  // Using Karger's random contraction algorithm
  def minCutSize(iterations: Int) : Int = (1 to iterations).foldLeft(Int.MaxValue) {(acc, sz) =>
    val sz = karger
    Math.min(acc, karger)
  }

  def contract(x: Int, y: Int) = {
    val m = Map[String, Int]()
    val g = Graph(n-1)
    val (lo,hi) = minMax(x, y)

    def newIndex(vi: Int) = if(vi > hi) vi-1 else vi

    for(h <- 0 until n; t <- adj(h)) {
      if(h == x || h == y) {
        // Head in compact
        if(t == x || t == y) {
          // Do nothing, self-loop
        } else {
          // Tail out of compact
          g.adj(lo) = newIndex(t)::g.adj(lo)
        }
      } else {
        // Head out of compact
        if(t == x || t == y) {
         // Tail in compact
          g.adj(newIndex(h)) = lo::g.adj(newIndex(h))
        } else {
          // Tail out of compact
          g.adj(newIndex(h)) = newIndex(t)::g.adj(newIndex(h))
        }
      }
    }
    g
  }

  // Naive implementation
  def karger = {
    var g = this
    val rng = new scala.util.Random()
    (n-2).times {
      val edge = g.edges(rng.nextInt(g.edges.length))
      g = g contract (edge._1, edge._2)
    }
    g.edgeCount
  }
}

