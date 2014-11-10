import org.scalatest.{Matchers, FunSpec}
import Knapsack._


class KnapsackSpec extends FunSpec with Matchers {
  describe("Knapsack") {
    it("is zero with large item") {
      val value = maxValue(0, Seq(Item(100, 100)))

      value should equal (0)
    }

    it("value of the only item that fits") {
      maxValue(2, Seq(Item(100, 1), Item(200, 3))) should equal (100)
    }
  }


}
