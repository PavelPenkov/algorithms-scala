import org.scalatest.{FunSpec, ShouldMatchers}

class HeapSpec extends FunSpec with ShouldMatchers {
  import RandomOps._
  describe("Binary heap") {
    it("stores elements") {
      val h = Heap[Int]
      h.put(1)

      val x = h.deleteMin

      x should equal(1)
    }

    it("three elements") {
      val h = Heap[Int]
      h.put(1)
      h.put(2)
      h.put(3)

      val x = h.deleteMin

      x should equal(1)
    }

    it("heap sort") {
      val n = 10
      val h = Heap[Int]
      val xs = randomInts(n)
      xs.foreach { x => h put x }
      h.toList should equal((1 to n).toList)
    }

    it("size with one element") {
      val h = Heap[Int]
      h put 1

      h.size should equal(1)

      h deleteMin

      h.size should equal(0)
    }

    it("size with multiple elements") {
      val h = Heap[Int]
      (1 to 10).foreach(h.put)

      h.size should equal(10)
    }
  }
}
