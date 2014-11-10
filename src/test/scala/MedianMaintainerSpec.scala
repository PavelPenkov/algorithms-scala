import org.scalatest.{FunSpec, ShouldMatchers}
import scala.util.Random

class MedianMaintainerSpec extends FunSpec with ShouldMatchers {
  def fixture = MedianMaintainer[Int]

  describe("A median maintainer") {
    it("single") {
      val mm = fixture
      mm put 1

      mm.percentile should equal(1)
    }

    it("odd") {
      val mm = fixture
      mm put 1
      mm put 2
      mm put 3

      mm.percentile should equal(2)
    }

    it("odd reverse") {
      val mm = fixture
      mm put 3
      mm put 2
      mm put 1

      mm.percentile should equal(2)
    }

    it("large odd") {
      val mm = fixture
      mm.put(1).put(2).put(3).put(4).put(5)

      mm.percentile should equal (3)

    }

    it("large even") {
      val mm = fixture
    }
  }
}
