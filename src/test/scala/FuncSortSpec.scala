package me.penkov.inversions

import org.scalatest._
import scala.util.Random

class FuncSortSpec extends FunSpec with Matchers {
  val rng = new Random()

  describe("Functional merge") {
    it("sorted streams") {
      val l = Stream(1, 3, 5)
      val r = Stream(2, 4, 6)

      FuncSort.streamMerge(l, r) should equal(Stream(1, 2, 3, 4, 5, 6))
    }

    it("large lists") {
      val l = 1.to(100000, 2).toList
      val r = 2.to(100000, 2).toList

      FuncSort.listMerge(l, r)
    }

    it("large streams") {
      val l = 1.to(100000, 2).toStream
      val r = 2.to(100000, 2).toStream

      FuncSort.streamMerge(l, r)
    }
  }

  describe("Functional sort") {
    it("sorted stream") {
      val xs = Stream.from(1) take 10
      val expected  = Stream.from(1) take 10

      FuncSort.streamSort(xs) should equal(expected)
    }

    it("reversed stream") {
      val xs = Stream.from(1).take(100000).reverse
      val expected = Stream.from(1) take 100000

      FuncSort.streamSort(xs) should equal(expected)
    }

    it("random stream") {
      val n = 20 * 1000
      val xs = rng.shuffle((1 to n).toList).toStream
      val expected = Stream.from(1) take n

      FuncSort.streamSort(xs) should equal(expected)
    }
  }

  describe("Functional inversion counter") {
    it("lazy stream split") {
      val xs = Stream.from(1)

      val (l, r) = FuncSort.lazySplit(xs)
    }
    it("single inversion") {
      val l = Stream(1, 3)
      val r = Stream(2, 4)

      val (i, m) = FuncSort.trMergeAndCount(l, r)

      i should equal(1)
    }

    it("reversed stream") {
      FuncSort.sortAndCount(Stream(4, 3, 2, 1))._1 should equal (6)
    }

    it("single inversion large list") {
      val n = 100
      for(k <- 1 to 100) {
        val xs = (1 to n).toArray
        val i = rng.nextInt(n - 2)
        val tmp = xs(i)
        xs(i) = xs(i+1)
        xs(i+1) = tmp

        FuncSort.sortAndCount(xs.toStream)._1 should equal(1)
      }
    }
  }
}
