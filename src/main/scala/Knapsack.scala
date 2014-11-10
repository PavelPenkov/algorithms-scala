import scala.io._
import scala.concurrent._
import me.penkov.utils.Benchmark._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object Knapsack extends App {
  case class Item(value: Long, weight: Long)

  def fromFile(filename: String) = {
    val src = Source.fromFile(filename).getLines()
    val maxWeight = src.next().split("\\s+")(0).toLong
    val items = src.map { s => s.split("\\s+") match { case Array(v, w) => Item(v.toInt, w.toInt) }}.toSeq
    (maxWeight, items)
  }

  def maxValueFuture(size: Long, items: Seq[Item]) = {
    def maxValueIter(size: Long, start: Int): Future[Long] = {
      if (start == items.length -1) {
        if(items.last.weight <= size) Future.successful(items.last.value) else Future.successful(0l)
      } else {
        for { v1 <- maxValueIter(size - items(start).weight, start + 1)
              v2 <- maxValueIter(size, start + 1)
        } yield math.max(v1 + items(start).value, v2)
      }
    }

    maxValueIter(size, 0)
  }

  def maxValueCached(size: Long, items: Seq[Item]) = {
    val cache = Array.fill(items.size) { collection.mutable.HashMap[Long, Long]()}
    def maxValueIter(size: Long, start: Int): Long = {
      val result = cache(start).get(size) getOrElse {
        // println(s"start $start size $size")
        val item = items(start)
        if (start == items.length - 1) {
          if (item.weight <= size) item.value else 0l
        } else {
          val newSize = size - item.weight
          if (newSize >= 0) {
            math.max(item.value + maxValueIter(size - item.weight, start + 1), maxValueIter(size, start + 1))
          }
          else maxValueIter(size, start + 1)
        }
      }
      cache(start).put(size, result)
      result
    }

    maxValueIter(size, 0)
  }

  def maxValue(size: Long, items: Seq[Item]) = {
    def maxValueIter(size: Long, start: Int): Long = {
        // println(s"start $start size $size")
      val item = items(start)
      if (start == items.length - 1) {
        if (item.weight <= size) item.value else 0l
      } else {
        val newSize = size - item.weight
        if (newSize >= 0) {
          math.max(item.value + maxValueIter(size - item.weight, start + 1), maxValueIter(size, start + 1))
        }
        else maxValueIter(size, start + 1)
      }
    }

    maxValueIter(size, 0)
  }

  val (size, items) = fromFile("knapsack_big.txt")
  val ts = realtime {
    val value = maxValueCached(size, items)
    println(s"Value $value")
  }
  println(ts)
}
