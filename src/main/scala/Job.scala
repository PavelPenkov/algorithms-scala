import Numeric.Implicits._

case class Job[a : Numeric](weight: a, length: a) {
  def ratio = weight.toDouble / length.toDouble
}

object Job {
  class JobList[a : Numeric](jobs: List[Job[a]]) {
    val numeric = implicitly[Numeric[a]]

    def scheduleByDifference = jobs.sorted(new Ordering[Job[a]] {
      def compare(x: Job[a], y: Job[a]) = {
        val dx = x.weight - x.length
        val dy = y.weight - y.length
        (if (dx == dy) x.weight - y.weight else dx - dy).signum()
      }
    }.reverse)

    def scheduleByRatio = jobs.sortBy(_.ratio).reverse

    def weightedCompletionTime = {
      val cts = jobs.scanLeft (numeric.zero) { (ct, job) => ct + job.length}.tail
      jobs.zip(cts).foldLeft(numeric.zero) { case (wct, (job, ct)) => wct + job.weight * ct}
    }
  }

  implicit def jobList[a : Numeric](jobs: List[Job[a]]): JobList[a] = new JobList(jobs)

}
