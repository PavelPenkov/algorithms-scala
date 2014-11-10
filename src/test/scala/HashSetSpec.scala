import org.scalatest.{Matchers, FunSpec}

class HashSetSpec extends FunSpec with Matchers {
  import RandomOps._
  def fixture = HashSet[Int]

  describe("A hash set") {
    it("empty contains nothing") {
      fixture contains 0 should be (false)
    }

    it("contains what was put before") {
      val hs = fixture
      hs+=1

      hs contains 1 should be (true)
    }

    it("can resize") {
      val hs = fixture
      val n = 1000
      randomInts(n) foreach { x => hs+=x }

      (1 to n) foreach { x => hs contains x should be (true)}
    }
  }

}
