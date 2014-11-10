package pure

object Digraph {
  def apply(n: Int) = new Digraph(n, Vector.fill(n)(List.empty[Int]))
}

class Digraph private (val n: Int, val adj: Vector[List[Int]])  {
  def addEdge(u: Int, v: Int) = new Digraph(n, adj.updated(u, v :: adj(u)))

  def postOrder(s: Int) = {
    def _postOrder(s: Int, state: (List[Int], Vector[Boolean])): (List[Int], Vector[Boolean]) = {
      val (ordered, visited) = state
      val newVisited = visited.updated(s, true)
      val reachable = adj(s).filterNot(newVisited).foldLeft((ordered, newVisited)){(acc, v) => _postOrder(v, acc)}
      (s::reachable._1, reachable._2)
    }

    _postOrder(s, (List[Int](), Vector.fill(n)(false)))._1
  }
}