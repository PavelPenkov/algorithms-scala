import org.scalatest.{FunSpec, ShouldMatchers}
import scala.util.Random

class ImpSpec extends FunSpec with ShouldMatchers {
  val rng = new Random()

  def checkPartition[a <% Ordered[a]](i: Int, xs: Array[a]) = {
    assert(xs.slice(0, i).forall(_ < xs(i)))
    assert(xs.drop(i+1).forall(_ > xs(i)))
  }

  describe("Imperative partition") {
    it("three elements middle") {
      val xs = Array(2, 1, 3)
      val p = ImpSort.partition(xs, 0, 2)

      p should equal(1)
      xs should equal(Array(1, 2, 3))
    }

    it("three elements middle reverse") {
      val xs = Array(2, 3, 1)
      val p = ImpSort.partition(xs, 0, 2)

      p should equal(1)
      xs should equal(Array(1, 2, 3))
    }

    it("three elements first") {
      val xs = Array(1, 2, 3)
      val p = ImpSort.partition(xs, 0, 2)

      p should equal(0)
      xs should equal(Array(1, 2, 3))
    }

    it("three elements last") {
      val xs = Array(3, 1, 2)
      val p = ImpSort.partition(xs, 0, 2)

      p should equal(2)
      checkPartition(p, xs)
    }

    it("largish odd") {
      val n = 11
      val k = 10
      for(x <- 1 to k) {
        val xs = rng.shuffle(0 to n).toArray // IDEA bug workaround
        val p = ImpSort.partition(xs, 0, xs.length - 1)
        checkPartition(p, xs)
      }
    }

    it("largish even") {
      val n = 12
      val k = 10
      for(x <- 1 to k) {
        val xs = rng.shuffle(0 to n).toArray
        val pivot = xs(0)
        val p = ImpSort.partition(xs, 0, xs.length - 1)
        checkPartition(p, xs)
      }
    }
  }

  describe("Imperative quick sort") {
    it("one element") {
      val xs = Array(1)

      ImpSort.qsortHead(xs)

      xs should equal(Array(1))
    }

    it("three element") {
      val xs = Array(3, 2, 1)

      ImpSort.qsortHead(xs)

      xs should equal(Array(1,2,3))
    }

    it("random") {
      val xs = rng.shuffle(1 to 10).toArray

      // println(xs mkString ",")

      ImpSort.qsortHead(xs)

      // println(xs mkString ",")

      val expected = (1 to 10).toArray

      xs should equal(expected)
    }
  }
}
