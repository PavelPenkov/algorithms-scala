import scala.collection.mutable
import scala.util.control.Breaks._
import scala.io.Source
import scala.None

object Digraph {
  def apply(n: Int) = new Digraph(n)

  def fromFile(filename: String) = {
    val src = Source fromFile filename getLines
    val n = src.next().toInt
    val g = new Digraph(n)
    for (s <- src) {
      s split "\\s+" map (_.toInt) match {
        case Array(u, v) => g.addEdge(u - 1, v - 1)
      }
    }
    g
  }
}

class Digraph private(val n: Int) {
  private val adj = Array.fill(n) {
    List.empty[Int]
  }
  private var m = 0

  def addEdge(u: Int, v: Int) = {
    adj(u) = v :: adj(u)
    m += 1
    this
  }

  def M = m

  def dfsLoop(order: Iterable[Int])(f: (Int, Int) => Unit) {
    val visited = Array.fill(n)(false)
    val stack = mutable.Stack[Int]()
    for (v <- order) {
      if (!visited(v)) {
        stack.push(v)
        while (!stack.isEmpty) {
          val c = stack.head
          visited(c) = true
          val toVisit = adj(c).filterNot(visited)
          if (toVisit.isEmpty) {
            stack.pop()
            f(c, v)
          } else {
            stack.push(toVisit.head)
          }
        }
      }
    }
  }

  def dfs = {
    val order = mutable.Queue[Int]()
    val visited = Array.fill(n) {
      false
    }
    val stack = mutable.Stack[Int]()

    for (start <- 0 until n) {
      if (!visited(start)) {
        stack.push(start)

        while (!stack.isEmpty) {
          val v = stack.pop()
          order.enqueue(v)
          visited(v) = true // only needed for start
          val toVisit = adj(v).filterNot(visited)
          for (w <- toVisit) {
            visited(w) = true
            stack.push(w)
          }
        }
      }
    }
    order
  }

  // Gabow's path-base strongly connected components
  def sccPathBased: Seq[Int] = {
    val s = mutable.Stack[Int]()
    val b = mutable.Stack[Int]()
    val i = Array.fill(n) { -1 }
    val visited = Array.fill(n) {
      false
    }
    var c = n - 1

    def dfs(v: Int): Unit = {
      s.push(v)
      i(v) = s.size - 1
      b.push(i(v))
      for (w <- adj(v)) {
        if (i(w) == -1) {
          dfs(w)
        } else {
          while (i(w) < b.head) {
            b.pop()
          }
        }
      }
      if (i(v) == b.head) {
        b.pop()
        c += 1
        while (i(v) <= s.size - 1) {
          i(s.pop()) = c
        }
      }
    }

    for (v <- 0 until n) {
      if (i(v) == -1) dfs(v)
    }

    i.toSeq
  }

  def reversed: Digraph = {
    val g = Digraph(n)
    for (v <- 0 until n; w <- adj(v)) {
      g.addEdge(w, v)
    }
    g
  }

  // Strongly connected components using Kosaraju's algorithm
  def sccKosaraju: Seq[Int] = {
    val finishingTimes = Array.fill(n)(0)
    var t = 0
    val leaders = Array.fill(n)(0)
    reversed.dfsLoop(0 until n) { (x, _) =>
      finishingTimes(x) = t
      t += 1
    }
    dfsLoop((0 until n).sortBy { x => -finishingTimes(x)}) { (x, leader) => leaders(x) = leader}
    leaders
  }


  // Strongly connected components using Tarjan's algorithm
  def sccTarjan: Seq[Int] = {
    val marked = Array.ofDim[Boolean](n)
    val stack = mutable.Stack[Int]()
    val id = Array.ofDim[Int](n)
    val low = Array.ofDim[Int](n)
    var pre = 0
    var count = 0


    def sccIter(v: Int): Unit = {
      marked(v) = true
      pre += 1
      low(v) = pre
      var min = low(v)
      stack.push(v)
      for (w <- adj(v)) {
        if (!marked(w)) sccIter(w)
        if (low(w) < min) min = low(w)
      }
      if (min < low(v)) {
        low(v) = min
        return
      }
      var w = 0
      do {
        w = stack.pop()
        id(w) = count
        low(w) = n
      } while (w != v)
      count += 1
    }

    for (i <- 0 until n) {
      if (!marked(i)) {
        sccIter(i)
      }
    }
    id.toSeq
  }
}
