package me.penkov.inversions

import org.scalatest.FunSpec
import scala.util.Random
import me.penkov.utils.Benchmark

class FuncSortBench extends FunSpec {
  val rng = new Random()
  describe("Merge sort using streams") {
    for(k <- (10 to 18).map{ Math.pow(2, _).toInt }) {
      it(s"full stream sort $k") {
        val xs = rng.shuffle((1 to k).toList).toStream

        val ts = Benchmark.realtime {
          // println(FuncSort.streamSort(xs).last)
        }

        // info(s"Full sort: $k elements in $ts ms")
      }
    }

    for(k <- (10 to 18).map{ Math.pow(2, _).toInt }) {
      it(s"first element only $k") {
        val xs = rng.shuffle((1 to k).toList).toStream

        val x = FuncSort.streamSort(xs).head

        // info(s"Min only: $k elements in $ts ms")
      }
    }
  }
}
