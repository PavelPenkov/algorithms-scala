class UnionFind(val n: Int) {
  val parent = (0 until n).toArray
  val rank = Array.fill(n)(0)
  var components = n

  def connected(x: Int, y: Int) = find(x) == find(y)

  def find(x: Int): Int = {
    if(x != parent(x))  parent(x) = find(parent(x))
    parent(x)
  }

  def union(x: Int, y: Int) {
    val px = find(x)
    val py = find(y)
    if (px != py) {
      link(px, py)
      components-=1
    }
  }

  def link(x: Int, y: Int) {
    if(rank(x) > rank(y)) {
      parent(y) = x
    } else {
      parent(x) = y
      if(rank(x) == rank(y)) rank(y)+=1
    }
  }

  def areConnected(x: Int, y: Int) = find(x) == find(y)
}

object UnionFind {
  def apply(n: Int) = new UnionFind(n)
}
