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

  def fromBitString(s: String): BitString =  Integer.parseInt(s.replaceAll("\\s+", ""), 2)

  def hammingDistance(x: Int, y: Int) = {
    var dist = 0
    var value = x ^ y
    while(value != 0) {
      dist+=1
      value = value & (value -1)
    }
    dist
  }


  val src = Source.fromFile("clustering_big.txt")
  val lines = src.getLines()
  val length = lines.next().split("\\s+").map(_.toInt) match {
    case Array(_, l) => l
  }
  val coords = lines.map(fromBitString).toIndexedSeq
  src.close()
  val coordsToVertex = coords.zipWithIndex.groupBy(_._2).mapValues(_.unzip._2).withDefaultValue(List.empty[Int])
  val uf = UnionFind(coords.size)
  val spacing = 3
  for { spc <- 0 until spacing
    i <- 0 until coords.length
    neighborCoords <- neighbors(coords(i), spc, length)
    neighbor <- coordsToVertex(neighborCoords)
  } {
    uf.union(i, neighbor)
  }
  println(s"${uf.components}")
}
