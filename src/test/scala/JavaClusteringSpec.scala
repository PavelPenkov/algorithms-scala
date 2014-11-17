import org.scalatest._
import scala.collection.JavaConversions._

class JavaClusteringSpec extends FunSpec with Matchers {
  describe("Clustering in Java") {
    it("computes Hamming distance") {
      val bs = new BitString(4)

      bs.hammingDistance(0, 15) should equal (4)
    }

    it("finds neighbors short") {
      val bs = new BitString(4)

      val ns = bs.neighbors(2, 2).toList


    }

    it("finds neighbors long") {
      val bs = new BitString(24)

      val rng = new scala.util.Random()
      val x = rng.nextInt(math.pow(2, 24).toInt)

      val neighbors = bs.neighbors(x, 3).toList

      neighbors should have length (2024)

      neighbors.forall(y => bs.hammingDistance(y, x) == 3) should be (true)
    }

    it("converts string to integer") {
      val s = "1 1 1 0 0 0 0 0 1 1 0 1 0 0 1 1 1 1 0 0 1 1 1 1 "

      JavaClustering.fromBitString(s) should equal (14734287)
    }
  }

}
