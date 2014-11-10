package me.penkov.inversions

import scala.io.Source
import me.penkov.utils.Benchmark

object Program {
  def realtime(f: => Unit) = {
    val start = System.currentTimeMillis
    f
    System.currentTimeMillis() - start
  }

  def main(args: Array[String]) = {
    val xs = Source.fromFile(args(0)).getLines().map{_.toInt}.toArray
    val ys = xs.clone.toStream
    var ts = realtime { println(BadCounter.countInversions(xs)) }
    println(s"Scala completed in $ts ms")

    ts = realtime { println(FuncSort.streamSort(ys).last)}
    println(s"Stream sort completed in $ts ms")

  }
}
