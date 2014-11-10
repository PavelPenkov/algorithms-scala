import scala.io._

class WeightedDigraph(val n: Integer) {
  case class Edge(from: Int, to: Int, weight: Long)

  val adj = Array.fill(n)(List.empty[Edge])

  var m = 0

  def addEdge(from: Int, to: Int, w: Long) = {
    adj(from) = Edge(from, to, w)::adj(from)
    m+=1
    this
  }

  // Using Dijkstra's algorithm
  def shortestDistances(s: Int) = {
    val heap = IndexedHeap[Long](n)

    def put(v: Int, w: Long) {
      heap.put(v+1, w)
    }

    def deleteMin = heap.deleteMin match { case IndexKeyPair(v, w) => IndexKeyPair(v-1, w) }

    def changeKey(v: Int, w: Long) { heap.changeKey(v+1, w) }

    def contains(v: Int) = heap.contains(v+1)

    def get(v: Int) = heap.get(v+1)

    val distances = Array.fill(n)(Long.MaxValue)
    distances(s) = 0

    (0 until n) foreach { v => put(v, if (v==s) 0 else Long.MaxValue)}

    while(!heap.isEmpty) {
      val pair = deleteMin
      val v = pair.index
      val weight = pair.key
      distances(v) = weight
      adj(v) filter { edge => contains(edge.to) } foreach { edge =>
        val newDist = distances(v) + edge.weight
        if (get(edge.to) > newDist)  changeKey(edge.to, newDist)
      }
    }

    distances
  }

  def floydWarshal: Either[Int, Array[Array[Long]]] = {
    var prev = Array.ofDim[Long](n, n)
    val current = Array.ofDim[Long](n, n)
    // Base case, budget is zero
    for (i <- 0 until n; j <- 0 until n) {
      prev(i)(j) = if (i == j) 0l
      else {
        adj(i).find(_.to == j).map(_.weight) getOrElse Long.MaxValue
      }
    }
    for (k <- 0 until n) {
      for (i <- 0 until n; j <- 0 until n) {
        val inherited = prev(i)(j)
        val part1 = prev(i)(k)
        val part2 = prev(k)(j)
        val composite = if(part1 == Long.MaxValue || part2 == Long.MaxValue) Long.MaxValue else part1 + part2
        current(i)(j) = math.min(inherited, composite)
        if (i == j && current(i)(j) < 0) return Left(i)
      }
      prev = current
    }
    Right(current)
  }
}

object WeightedDigraph {
  def apply(n: Int) = new WeightedDigraph(n)

  def fromFile(filename: String) = {
    val lines = Source.fromFile(filename).getLines()
    val n = lines.next().split("\\s+")(0).toInt
    val wg = new WeightedDigraph(n)
    lines foreach { line =>
      line split "\\s+" match {
        case Array(u,v,w) => wg.addEdge(u.toInt - 1, v.toInt - 1, w.toInt)
      }
    }
    wg
  }
}
