package me.penkov.inversions

import org.scalatest.path
import org.scalatest.matchers.ShouldMatchers
import scala.util.Random

class QsortSpec extends path.FunSpec with ShouldMatchers {
  val rng = new Random()
  describe("functional quick sort with head pivot") {
    it("should sort a stream with head pivot") {
      val xs = rng.shuffle(Stream.from(1).take(500))

      FuncSort.qsortHead(xs).shouldEqual(xs.sorted)
    }

    it("should sort a stream with last pivot") {
      val xs = rng.shuffle(Stream.from(1).take(500))

      FuncSort.qsortLast(xs).shouldEqual(xs.sorted)
    }

    it("should sort a stream with random pivot") {
      val xs = rng.shuffle(Stream.from(1).take(500))

      FuncSort.qsortRandom(xs).shouldEqual(xs.sorted)
    }

    it("should sort a stream with median of three pivot") {
      val xs = rng.shuffle(Stream.from(1).take(500))

      FuncSort.qsortRandom(xs).shouldEqual(xs.sorted)
    }

    it("counts the number of comparisons") {

      for(n <- List(10000, 20000, 40000)) {
        val xs = rng.shuffle(Stream.from(1).take(n))
        /*
        println(s"$n elements head")
        println(FuncSort.qsortAndCountHead(xs)._1)

        println(s"$n elements random")
        println(FuncSort.qsortAndCountRandom(xs)._1)
        */
      }
    }

    it("tail recursively partitions the stream") {
      val n = 10 * 1000
      val xs = rng.shuffle(Stream.from(1).take(n))
      val pivot = n/2
      val p = FuncSort.partitionBy(xs, pivot)

      p._1.forall(_ < pivot) should be(true)
      p._2.forall(_ > pivot) should be(true)
    }
  }
}
