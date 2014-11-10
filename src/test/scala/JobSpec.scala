import org.scalatest._

class JobSpec extends FunSpec with Matchers {
  import Job._
  describe("A job") {
    it("calculates weighted completion time equal weight") {
      val jobs = List(Job(1, 1), Job(1, 1), Job(1, 1))

      jobs.weightedCompletionTime should equal(6)
    }

    it("calculates weighted completion time different weight") {
      val jobs = List(Job(1, 1), Job(2, 2), Job(3, 3))

      jobs.weightedCompletionTime should equal(25)
    }

    it("breaks ties by weight") {
      val jobs = List(Job(1, 1), Job(2, 2), Job(3, 3))

      jobs.scheduleByDifference should equal(List(Job(3, 3), Job(2, 2), Job(1, 1)))
    }
  }
}
