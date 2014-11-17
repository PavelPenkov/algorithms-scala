import org.scalatest._
class ClusteringSpec extends FunSpec with Matchers {
  describe("Scala clustering") {
    it("finds neighbors short") {
      val ns = BigClustering.neighbors(2, 2, 4)
    }

    it("finds neighbors long") {
      val rng = new scala.util.Random()
      val x: BigClustering.BitString = rng.nextInt(math.pow(2, 24).toInt)

      val neighbors = BigClustering.neighbors(x, 3, 24)

      neighbors should have length (2024)

      neighbors.forall(y => BigClustering.hammingDistance(y, x) == 3) should be (true)
    }
  }

}
