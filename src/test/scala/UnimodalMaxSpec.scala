import java.util.Random
import org.scalatest.{ShouldMatchers, FunSpec}

class UnimodalMaxSpec extends FunSpec with ShouldMatchers {
  val rng = new Random()

  val incStream: Stream[Int] = 0 #:: 1 #:: incStream.zip(incStream.tail).map{n => n._2 + rng.nextInt(100)}


  def generateUnimodalArray(inc: Int, dec: Int) = {
    val incs = incStream take inc
    val decs = incStream take dec

    (Math.max(incs.max, decs.max), incs ++ decs.reverse)
  }

  describe("unimodal max") {
    it("three") {
      val xs = Array(1, 2, 1)

      TheoryProblems.unimodalMax(xs) should equal(2)
    }

    it("largish") {
      val xs = Array(1, 2, 3, 2, -1, -2, -3, -4, -5)

      TheoryProblems.unimodalMax(xs) should equal(3)
    }

    it("random") {
      val (max, xs) = generateUnimodalArray(300, 300)

      TheoryProblems.unimodalMax(xs.toArray) should equal(max)
    }
  }
}
