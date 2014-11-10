import java.util.Random
import org.scalatest.FunSpec

class DigraphSpec extends FunSpec with org.scalatest.ShouldMatchers {
  val rng = new Random()
  def randomGraph(n: Int, p: Double) = {
    val pairs = for(i <- 0 until n; j <- 0 until n) yield (i, j)
    pairs.foldLeft(Digraph(n)){(g, vs) => if(rng.nextDouble() < p) g.addEdge(vs._1, vs._2) else g}
  }

  describe("Directed graph") {
    ignore("has the right postorder") {
      val dg = Digraph(3).addEdge(0, 1).addEdge(1, 2)
    }

    ignore("sets correct finishing times") {
      val dg = Digraph(9).addEdge(0, 6)
        .addEdge(6, 3)
        .addEdge(3, 0)
        .addEdge(6, 8)
        .addEdge(8, 5)
        .addEdge(5, 2)
        .addEdge(2, 8)
        .addEdge(5, 7)
        .addEdge(7, 1)
        .addEdge(1, 4)
        .addEdge(4, 7)
        .reversed

      //println(dg.sccRec mkString ",")
    }

    ignore("sets correct finishing times in loop") {
      val dg = Digraph(9).addEdge(0, 6)
        .addEdge(6, 3)
        .addEdge(3, 0)
        .addEdge(6, 8)
        .addEdge(8, 5)
        .addEdge(5, 2)
        .addEdge(2, 8)
        .addEdge(5, 7)
        .addEdge(7, 1)
        .addEdge(1, 4)
        .addEdge(4, 7)
        .reversed

      dg.scc
    }

    it("simple graph") {
      val dg = Digraph(4).addEdge(0, 2)
        .addEdge(1, 0)
        .addEdge(1, 3)

      dg.scc
    }
  }
}
