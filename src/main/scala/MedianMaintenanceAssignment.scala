import scala.io._

object MedianMaintenanceAssignment extends App {
  val mm = MedianMaintainer[Int]
  var sum = 0
  Source.fromFile("Median.txt").getLines().foreach { x =>
    mm.put(x.toInt)
    sum+=mm.percentile
  }
  println(sum % 1000)
}
