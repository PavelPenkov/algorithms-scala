import org.scalatest._
import JavaClustering._

class ClusteringSpec extends FunSpec with Matchers {
  describe("Clustering algorithm") {
    it("should convert string to int simple") {
      val s = "1 1 1"
      fromBitString(s) should equal (7)
    }

    it("should convert string to int 24") {
      val s = "1 0 0 0 0 0"

      fromBitString(s) should equal (32)
    }

    it("finds neighbors at distance 3") {
      val x = 15;

      neighbors(15, 3, 4).size() should equal (4);
    }

    it("counts clusters 3") {
      val vertices = Array(
        Integer.parseInt("000000", 2),
        Integer.parseInt("111111", 2),
        Integer.parseInt("000111", 2)
      )

      clusters(vertices, 6, 3) should equal(3);
    }

    it("neighbors are distinct") {
      val ns = neighbors(0, 3, 24)


      ns.size should equal (2024)

      val set = (for(i <- 0 until ns.size()) yield ns.get(i).toInt).toSet

      set.size should equal (2024)
    }



  }
}
