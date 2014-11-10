import scala.collection.mutable
import scala.util.Sorting
import scala.io._


class WeightedGraph(val n: Int) {
  case class Edge(to: Int, weight: Long)

  case class DirectedEdge(from: Int, to: Int, weight: Long)

  val adj = Array.fill(n) { List.empty[Edge] }

  var m = 0

  def addEdge(u: Int, v: Int, weight: Int) =  {
    adj(u) = Edge(v, weight)::adj(u)
    adj(v) = Edge(u, weight)::adj(v)
    m+=1
    this
  }

  // Using Dijkstra algorithm
  def shortestDistances(s: Int) = {
    val heap = IndexedHeap[Long](n)
    val distances = Array.fill(n)(Long.MaxValue)
    distances(s) = 0

    (0 to n) foreach { v => heap.put(v, if (v==s) 0 else Long.MaxValue)}

    while(!heap.isEmpty) {
      val pair = heap.deleteMin
      val v = pair.index
      val weight = pair.key
      distances(v) = weight
      adj(v) filter { edge => heap contains edge.to } foreach { edge =>
        val newDist = distances(v) + edge.weight
        if (heap.get(edge.to) > newDist)  heap changeKey (edge.to, newDist)
      }
    }

    distances
  }

  // Using Prim's algorithm
  def mstWeightPrim = {
    val heap = IndexedHeap[Long](n)
    heap put (0, 0)

    for(x <- 1 until n) { heap.put(x, Long.MaxValue) }

    var weight = 0l

    while(!heap.isEmpty) {
      val pair = heap.deleteMin
      val (v, c) = (pair.index, pair.key)
      weight+=c
      adj(v) filter { edge => heap contains edge.to } foreach { edge =>
        if(heap.get(edge.to) > edge.weight) heap changeKey (edge.to, edge.weight)
      }
    }
    weight
  }

  def depthFirstOrder(s: Int) = {
    val visited = Array.fill(n)(false)
    val ordered = mutable.Queue[Int]()
    val stack = mutable.Stack[Int]()
    stack push s
    while(stack.nonEmpty) {
      val v = stack.pop()
      visited(v) = true
      val adjacent = adj(v).map(_.to).filterNot(visited)
      adjacent foreach { x =>
        visited(x) = true
        stack push x
      }
      ordered.enqueue(v)
    }
    ordered.toIterable
  }

  def mstWeightKruskal = {
    val edges = for { (edges, from) <- adj.zipWithIndex
      edge <- edges } yield DirectedEdge(from, edge.to, edge.weight)

    implicit val edgeOrdering = new Ordering[DirectedEdge] {
      override def compare(x: DirectedEdge, y: DirectedEdge): Int = x.weight.compare(y.weight)
    }
    Sorting.quickSort(edges)

    val uf = UnionFind(n)

    var weight: Long = 0
    var size = 0
    var i = 0
    while(size < n-1) {
      val edge = edges(i)
      if(!uf.areConnected(edge.from, edge.to)) {
        uf.union(edge.from, edge.to)
        size+=1
        weight+=edge.weight
      }
      i+=1
    }
    weight
  }

  def clusterSpacing(clusters: Int) = {
    val edges = for { (edges, from) <- adj.zipWithIndex
                      edge <- edges if from < edge.to } yield DirectedEdge(from, edge.to, edge.weight)

    implicit val edgeOrdering = new Ordering[DirectedEdge] {
      override def compare(x: DirectedEdge, y: DirectedEdge): Int = x.weight.compare(y.weight)
    }
    Sorting.quickSort(edges)

    val uf = UnionFind(n)

    var size = 0
    var i = 0
    while(size < n-clusters) {
      val edge = edges(i)
      if(!uf.areConnected(edge.from, edge.to)) {
        uf.union(edge.from, edge.to)
        size+=1
      }
      i+=1
    }
    edges.drop(i).find { edge => !uf.areConnected(edge.from, edge.to)}.map(_.weight) getOrElse 0
  }

}

object WeightedGraph {
  def apply(n: Int) = new WeightedGraph(n)

  def fromFile(filename: String) = {
    val lines = Source.fromFile(filename).getLines()
    val n = lines.next().split("\\s+")(0).toInt
    val wg = new WeightedGraph(n)
    lines foreach { line =>
      line split "\\s+" match {
        case Array(u,v,w) => wg.addEdge(u.toInt - 1, v.toInt - 1, w.toInt)
      }
    }
    wg
  }
}
