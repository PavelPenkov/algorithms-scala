import java.util.BitSet
import scala.io.Source

object BigClustering extends App {
  case class Edge(from: Int, to: Int, weight: Int)

  type BitString = Int

  def neighbors(s: BitString, dist: Int, length: Int) = {
    def flipped(i: Int, x: BitString) = x ^ (1 << i)

    def neighborsIter(s: BitString, dist: Int, start: Int): IndexedSeq[BitString] = if (dist == 0) Vector(s) else for {
      i <- start until length
      neighbor <- neighborsIter(flipped(i, s), dist-1, i+1)
    } yield neighbor

    neighborsIter(s, dist, 0)
  }


  def fromBitString(s: String): BitString =  s.split("\\s+").reverse.zipWithIndex.foldLeft(0) { case (acc, ("0",_)) => acc
      case (acc, ("1", index)) => acc + (1 << index)
  }



  def hammingDistance(x: Int, y: Int) = {
    var dist = 0
    var value = 0

    value = x ^ y

    while(value != 0) {
      dist+=1
      value = value & (value -1)
    }
    dist
  }


  //val strings = Source.fromFile("clustering_big.txt").getLines().drop(1).toList
  val n = 500
  val coords = Source.fromFile("clustering_big.txt").getLines().drop(1).take(n).map(fromBitString).toSeq

  val spacing = 3
  val uf = UnionFind(n)

  (for (i <- 0 until n;  j <- i until n) yield (i, j)).foreach { case (u, v) =>
    val dist = hammingDistance(coords(u), coords(v))
    if (dist < spacing) {
      if (u != v) println(s"Connecting $u at ${coords(u)} and $v at ${coords(v)} with dist $dist")
      uf.union(u, v)
    }
  }

  println(uf.components)




  /*
  val vertices = strings.zipWithIndex.map { case (s, i) =>
    val bitString: BitString = s.split("\\s+").zipWithIndex.foldLeft(0) {
      case (acc, ("0", _)) => acc
      case (acc, ("1", index)) => acc + (1 << index)
    }
    new {val coords = bitString; val index = i}
  }

  val vertexToCoords = scala.collection.mutable.Map[Int, BitString]()
  val coordsToVertex = scala.collection.mutable.Map[BitString, List[Int]]().withDefaultValue(List())


  vertices foreach { v =>
    vertexToCoords(v.index) = v.coords
    coordsToVertex(v.coords) = v.index::coordsToVertex(v.coords)
  }

  val uf = UnionFind(vertices.size)
  println(s"Size: ${vertices.size}")
  val dist = 3

  vertices foreach { i =>
    //println(s"Processing vertex ${i.index}")
    neighbors(i.coords, dist, 24) foreach { n =>
      coordsToVertex(n) foreach {v =>
        uf.union(i.index, v)
      }
    }
  }
  */
}
