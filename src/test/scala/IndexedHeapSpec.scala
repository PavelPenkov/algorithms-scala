import org.scalatest.{FunSpec, ShouldMatchers}

class IndexedHeapSpec extends FunSpec with ShouldMatchers {
  import RandomOps._

  describe("Intexed heap") {
    it("one element") {
      val h = IndexedHeap[String](1)
      h.put(0, "a")

      h.deleteMin should equal(IndexKeyPair(0, "a"))
    }
    it("Two elements") {
      val h = IndexedHeap[String](2)
      h.put(0, "b")
      h.put(1, "a")

      h.deleteMin should equal(IndexKeyPair(1, "a"))
    }

    ignore("Heap sort") {
      val n = 10
      val h = IndexedHeap[Int](n)

      shuffle(0 until n).foreach { x => h.put(x, x)}

      h.toList.map(_.key) should contain theSameElementsInOrderAs (0 until n)
    }

    it("three elements") {
      val h = IndexedHeap[Int](4)
      h.put(0, 0)
      h.put(1, 1)
      h.put(2, 2)
      h.put(3, 3)
      // h.put(4, 4)

      h.deleteMin.index should equal (0)
      h.deleteMin.index should equal (1)
      h.deleteMin.index  should equal (2)
    }

    it("heap sort") {
      val n = 10
      val h = IndexedHeap[Int](n)
    }


    it("decrease key") {
      val h = IndexedHeap[String](2)
      h.put(0, "c")
      h.put(1, "b")

      h.peek should equal(IndexKeyPair(1, "b"))

      h.changeKey(0, "a")

      h.peek should equal(IndexKeyPair(0, "a"))
    }

    it("increase key") {
      val h = IndexedHeap[String](2)
      h.put(0, "a")
      h.put(1, "b")

      h.peek should equal(IndexKeyPair(0, "a"))

      h.changeKey(0, "c")

      h.peek should equal(IndexKeyPair(1, "b"))
    }

    it("can't put index greater than size") {
      val h = IndexedHeap[Int](1)

      an [IndexOutOfBoundsException] should be thrownBy h.put(1, 0)
    }

    it("can't put index less than zero") {
      val h = IndexedHeap[Int](1)

      an [IndexOutOfBoundsException] should be thrownBy h.put(-1, 0)
    }
  }
}
