import org.scalatest._

class BitSetSpec extends FunSpec with Matchers {
  val bo = BitSetOps(4)

  describe("Bit set") {
    it("produces sets of cardinality one") {

      val sets = bo.ofCardinality(1)

      sets should have length 4

      sets.distinct should have length 4
    }

    it("produces sets of cardinality N") {
      val sets = bo.ofCardinality(4)

      sets should have length 1
    }

    it("produces set for TS cardinality N") {
      val sets = bo.forTsp(4, 2)

      sets should have length 1

      sets.head should equal (15)
    }

    it("produces set for TS cardinality 2") {
      val sets = bo.forTsp(2, 2)

      sets should have length 1

      sets.head should equal (5)
    }

    it("produces set for TS cardinality N-1") {
      val sets = bo.forTsp(3, 2)

      sets should have length 2

      sets should contain only (7, 13)
    }
  }
}
