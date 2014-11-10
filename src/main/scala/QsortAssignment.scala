import scala.io.Source

object QsortAssignment extends App {
  val xs = Source.fromFile(args(0)).getLines().map(_.toInt).toArray

  val xsh = xs.clone(); val xsl = xs.clone(); val xsm = xs.clone()

  val ch = ImpSort.qsortAndCountHead(xsh)
  println(s"Head pivot: $ch")

  val cl = ImpSort.qsortAndCountLast(xsl)
  println(s"Last pivot: $cl")

  val cm = ImpSort.qsortAndCountMedian(xsm)
  println(s"Median pivot: $cm")
}
