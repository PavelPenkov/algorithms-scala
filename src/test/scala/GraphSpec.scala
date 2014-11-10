import org.scalatest._
import Matchers._

class GraphSpec extends FunSpec {
  import GraphUtils._

  describe("undirected graph") {
    it("should enumerate edges 1") {
      val g = Graph(2)
      g.addEdge(0, 1)

      g.edges should contain only ((0, 1), (1,0))
    }

    it("should enumerate edges 2") {
      val g = Graph(3)
      g.addEdge(0, 1)
      g.addEdge(1, 2)
      g.addEdge(2, 0)

      g.edges should contain only ((0, 1), (1, 0), (1, 2), (2, 1), (2, 0), (0, 2))
    }

    it("contracts line 2") {
      val g = line(2)
      val c = g.contract(0, 1)

      c.n should equal(1)
      c.edges.length should equal(0)
    }

    it("contracts line 3") {
      val g = line(3)
      val c = g.contract(0, 1)

      c.n should equal(2)
      c.edges.length should equal(2)
    }

    it("returns a min cut size for a circle") {
      val g = circle(10)

      g.minCutSize(1) should equal(2)
    }

    it("returns a min cut size for a line") {
      val n = 100
      val g = line(10)

      g.minCutSize(1) should equal(1)
    }

    it("returns a min cut size for complete binary tree") {
      val g = fullTree(2)

      g.minCutSize(1) should equal(1)
    }
  }
}
