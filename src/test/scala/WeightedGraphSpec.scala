import org.scalatest.FunSpec
import org.scalatest.Matchers

class WeightedGraphSpec extends FunSpec with Matchers {
  describe("A weighted graph") {
    it("computes MST for line") {
      val wg = new WeightedGraph(2)
      wg addEdge(0, 1, 10)

      wg.mstWeightPrim should equal (10)
    }

    it("computes MST for circle") {
      val wg = new WeightedGraph(3)
      wg addEdge(0, 1, 1)
      wg addEdge(1, 2, 3)
      wg addEdge(2, 0, 5)

      wg.mstWeightPrim should equal (4)
    }

    it("computes MST four vertices") {
      val wg = new WeightedGraph(4)

      wg addEdge (0, 1, 1)
      wg addEdge (0, 2, 3)
      wg addEdge (1, 2, 2)
      wg addEdge (2, 3, 4)
      wg addEdge (3, 0, 5)

      wg.mstWeightPrim should equal (7)
    }

    it("computes spacing for line") {
      val wg = new WeightedGraph(5)

      wg addEdge (0, 1, 1)
      wg addEdge(1, 2, 2)
      wg addEdge(2, 3, 3)
      wg addEdge(3, 4, 4)

      // wg.clusterSpacing(2) should equal(4)
      wg.clusterSpacing(4) should equal(2)
      wg.clusterSpacing(5) should equal(1)
    }
  }
}
