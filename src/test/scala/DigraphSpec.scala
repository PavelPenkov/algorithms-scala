import java.util.Random
import org.scalatest._

class DigraphSpec extends FunSpec with Matchers {
  val rng = new Random()
  def randomGraph(n: Int, p: Double) = {
    val pairs = for(i <- 0 until n; j <- 0 until n) yield (i, j)
    pairs.foldLeft(Digraph(n)){(g, vs) => if(rng.nextDouble() < p) g.addEdge(vs._1, vs._2) else g}
  }

  describe("Directed graph") {
    it("Correctly searches full graph") {
      val n = 10
      val dg = Digraph(n)
      for(i <- 0 until n; j <- 0 until n) {
        dg.addEdge(i, j)
      }

      val vs = dg.dfs

      vs should have length 10
    }

    it("correctly searches disjoint graph") {
      val g = Digraph(4)
      g.addEdge(0, 1).addEdge(2, 3)

      val vs = g.dfs

      vs should have length 4
    }

    it("finds SCC circle") {
      val g = Digraph(3)
      g.addEdge(0, 1).addEdge(1, 2).addEdge(2, 0)

      val sccs = g.sccTarjan

      sccs should equal (List(0, 0, 0))
    }

    it("finds SCC") {
      val g = Digraph(4)
      g.addEdge(0, 1).addEdge(1, 2).addEdge(2,0).addEdge(2,3)

      g.sccTarjan should equal (List(1, 1, 1, 0))
    }

    it("path based line 2") {
      val g = Digraph(2)
      g.addEdge(0, 1)

      info(g.sccPathBased mkString ",")
    }

    it("path based circle 2") {
      val g = Digraph(2)
      g.addEdge(0, 1).addEdge(1, 0)

      info(g.sccPathBased mkString ",")
    }

    it("path based line 3") {
      val g = Digraph(3)
      g.addEdge(0,1).addEdge(1, 2)

      info(g.sccPathBased mkString ",")
    }

    it("path based circle 3") {
      val g = Digraph(3)
      g.addEdge(0, 1).addEdge(1, 2).addEdge(2, 0)

      info(g.sccPathBased mkString ",")
    }

    it("path based complex 1") {
      val g = Digraph(6)

      g.addEdge(0, 1).addEdge(1, 2).addEdge(2, 0).addEdge(2,3)
        .addEdge(3, 4).addEdge(4, 5).addEdge(5, 3)

      info(g.sccPathBased mkString ",")
    }

    it("path based medium") {
      val g = Digraph.fromFile("dg.txt")

      val ids = g.sccPathBased

      info(ids.zipWithIndex.groupBy(_._1).mapValues(_.length) mkString ",")
    }

    it("path based large") {
      val g = Digraph.fromFile("SCC.txt")

      val ids = g.sccPathBased

      info(ids.distinct.length.toString)
    }
  }
}
