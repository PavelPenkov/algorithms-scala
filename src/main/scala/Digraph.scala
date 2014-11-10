import scala.collection.mutable
import scala.io.Source
import scala.None

object Digraph {
  def apply(n: Int) = new Digraph(n, Vector.fill(n)(List.empty[Int]))
}

class Digraph private (val n: Int, val adj: Vector[List[Int]])  {
  def addEdge(tail: Int, head: Int) = new Digraph(n, adj.updated(tail, head :: adj(tail)))

  def addEdge(e: (Int, Int)): Digraph = addEdge(e._1, e._2)

  def postOrder(s: Int) = {
    val visited = Array.fill(n)(false)
    val ordered = mutable.Queue[Int]()

    def _dfs(v: Int) {
      adj(v).filterNot(visited).foreach(_dfs)
      ordered.enqueue(v)
    }

    _dfs(s)
    ordered.iterator
  }

  def postOrder = {
    val visited = Array.fill(n)(false)
    val ordered = mutable.Queue[Int]()

    def _dfs(v: Int) {
      visited(v) = true
      for(x <- adj(v)) {
        if(!visited(x))  _dfs(x)
      }
      ordered.enqueue(v)
    }

    for(v <- 0 until n) {
      if(!visited(v)) {
        _dfs(v)
      }
    }

    ordered
  }

  def dfsLoop(vs: Iterable[Int]): Iterable[Int] = {
    val visited = Array.fill(n)(false)
    val ordered = mutable.Queue[Int]()
    val stack = mutable.Stack[Int]()

    for(v <- vs) {
      if(!visited(v)) {
        stack.push(v)
        visited(v) = true
        while(!stack.isEmpty) {
          val c = stack.pop()

          if(adj(c).filterNot(visited).isEmpty) ordered.enqueue(c)

          for(n <- adj(c)) {
            if(!visited(n)) {
              visited(n) = true
              stack.push(n)
            }
          }
          if(stack.isEmpty) ordered.enqueue(c)
        }
      }
    }
    ordered
  }

  def dfsLoop: Iterable[Int] = dfsLoop(1 until n)

  def edges =  for(i <- 0 until n; head <- adj(i) ) yield (i, head)

  lazy val reversed = edges.foldLeft(Digraph(n)){ case(g, (tail, head)) => g addEdge (head, tail)}

  def sccRec = {
    val visited = Array.fill(n)(false)
    val finishingTimes = Array.fill(n)(0)
    val leaders = Array.fill(n)(0)
    var t = 0

    def _dfs(x: Int) {
      visited(x) = true
      reversed.adj(x) foreach { tail => if(!visited(tail)) _dfs(tail) }
      finishingTimes(x) = t
      t+=1
    }

    for(v <- 0 until n) {
      if(!visited(v)) _dfs(v)
    }
    finishingTimes
  }

  def dfsLoop2(f: Int => Unit) {
    val visited = Array.fill(n)(false)
    val stack = mutable.Stack[Int]()
    val vs = 0 until n
    for(v <- vs) {
      if(!visited(v)) {
        stack.push(v)
        while(!stack.isEmpty) {
          val c = stack.head
          visited(c) = true
          val toVisit = reversed.adj(c).filterNot(visited(_))
          if(toVisit.isEmpty) {
            stack.pop()
            f(c)
          } else {
            stack.push(toVisit.head)
          }
        }
      }
    }
  }

  def dfsLoop3(order: Iterable[Int])(f: (Int, Int) => Unit) {
    val visited = Array.fill(n)(false)
    val stack = mutable.Stack[Int]()
    for(v <- order) {
      if(!visited(v)) {
        stack.push(v)
        while(!stack.isEmpty) {
          val c = stack.head
          visited(c) = true
          val toVisit = adj(c).filterNot(visited(_))
          if(toVisit.isEmpty) {
            stack.pop()
            f(c, v)
          } else {
            stack.push(toVisit.head)
          }
        }
      }
    }

  }

  // Sizes of strongly connected components using Kosaraju's algorithm
  def scc = {
    val finishingTimes = Array.fill(n)(0)
    var t = 0
    val leaders = Array.fill(n)(0)

    reversed.dfsLoop3(0 until n) { (x,_) =>
      finishingTimes(x) = t
      t+=1
    }

    dfsLoop3((0 until n).sortBy { x => -finishingTimes(x) }) { (x, leader) => leaders(x) = leader }

    val m = mutable.Map[Int, Int]().withDefaultValue(0)
    leaders foreach { l => m(l) = m(l) + 1 }
    m.toList
  }
}
