import scala.io.Source

object SchedulingAssignment extends App {
  import Job._

  val jobs = Source.fromFile("jobs.txt").getLines() drop 1 map { line =>
    line split "\\s+" match {
      case Array(w, l) => Job(w.toInt, l.toInt)
    }
  }

  val jobList = jobs.toList

  val byRatio = jobList.scheduleByRatio

  val wctRatio = byRatio.weightedCompletionTime
  val wctDiff = jobList.scheduleByDifference.weightedCompletionTime

  println(s"Scheduled by ratio: $wctRatio")
  println(s"Scheduled by difference: $wctDiff")
  println(wctDiff - wctRatio)
}
