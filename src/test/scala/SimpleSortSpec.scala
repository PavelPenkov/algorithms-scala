package me.penkov.inversions

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import scala.util.Random

class SimpleSortSpec extends FunSpec with ShouldMatchers {
  val rng = new Random()
  describe("Imperative merge") {
    it("one element") {
      val xs = Array(1)

      val res = SimpleSort.merge(xs)

      res should equal(Array(1))
    }

    it("two elements") {
      val xs = Array(1, 2)

      SimpleSort.merge(xs) should equal(Array(1, 2))
    }

    it("three elements") {
      val xs = Array(1, 2, 3)

      SimpleSort.merge(xs) should equal(Array(1, 2, 3))
    }

    it("three elements unsorted") {
      SimpleSort.merge(Array(1, 3, 2)) should equal(Array(1, 2, 3))

      SimpleSort.merge(Array(2, 3, 1)) should equal(Array(1, 2, 3))
    }

    it("large array") {
      val xs = (1.to(10, 2) ++ 2.to(10, 2)).toArray

      val res = SimpleSort.merge(xs)
      res should equal((1 to 10).toArray)
    }
  }

  describe("Imperative merge sort") {
    it("one element") {
      val xs = Array(1)

      SimpleSort.mergeSort(xs)

      xs should equal(Array(1))
    }

    it("two elements") {
      val xs = Array(1, 2)

      SimpleSort.mergeSort(xs)
      xs should equal(Array(1, 2))
    }

    it("three elements") {
      val xs = Array(1, 2, 3)

      SimpleSort.mergeSort(xs)
      xs should equal(Array(1, 2, 3))
    }

    it("wtf") {
      val xs = Array(1, 2, 3)
      val aux = Array(0, 0, 0)

      SimpleSort.mergeSort(xs, aux, 1, 2)

    }

    it("three elements unsorted") {
      val xs = Array(3, 1, 2)
      SimpleSort.mergeSort(xs)
      xs should equal(Array(1, 2, 3))

      val ys = Array(2, 1, 3)
      SimpleSort.mergeSort(ys)
      ys should equal(Array(1, 2, 3))
    }

    it("large array") {
      val expected = (1 to 1000).toArray
      val xs = rng.shuffle(expected.toList).toArray

      SimpleSort.mergeSort(xs)

      xs should equal(expected)
    }
  }

  describe("Inversion counter") {
    it("sorted array") {
      val xs = (1 to 10).toArray

      SimpleSort.countInversions(xs) should equal(0)
    }

    it("single inversion") {
      val xs = Array(1, 3, 2)

      SimpleSort.countInversions(xs) should equal(1)
    }

    it("single inversion in large array") {
      val n = 100
      for(k <- 1 to 100) {
        val xs = (1 to n).toArray
        val i = rng.nextInt(n - 2)
        val tmp = xs(i)
        xs(i) = xs(i+1)
        xs(i+1) = tmp

        SimpleSort.countInversions(xs) should equal(1)
      }
    }

    it("reverse array") {
      val n = 100
      val xs = (1 to 100).reverse.toArray

      val expected = (n - 1) * n / 2

      SimpleSort.countInversions(xs) should equal(expected)
    }
  }
}
