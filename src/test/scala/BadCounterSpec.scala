package me.penkov.inversions

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class BadCounterSpec extends FunSpec with ShouldMatchers {
  describe("Bad inversion counter") {
    it("sorted array") {
      val xs = Array(1, 2, 3, 4)

      BadCounter.countInversions(xs) should equal(0)
    }

    it("reversed array") {
      BadCounter.countInversions(Array(4, 3, 2, 1)) should equal(6)
    }
  }
}
