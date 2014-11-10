package me.penkov.inversions
import scala.collection.immutable.Stream.Empty
import scala.annotation.tailrec
import java.util.Random

class Qsort {
  var comparisons = 0

  private def qsortAndCount[a <% Ordered[a]](xs: Stream[a])(choosePivot:Stream[a] => a): Stream[a] = {
    if(xs.lengthCompare(1) <= 0) xs
    else {
      val pivot = choosePivot(xs)
      val l = xs.filter(_ < pivot)
      val r = xs.filter(_ > pivot)
      comparisons+=xs.length
      qsortAndCount(l)(choosePivot) ++ pivot#::qsortAndCount(r)(choosePivot)
    }
  }

  def qsortAndCountHead[a <% Ordered[a]](xs: Stream[a]) = qsortAndCount(xs)(_.head)

  def qsortAndCountLast[a <% Ordered[a]](xs: Stream[a]) = qsortAndCount(xs)(_.last)

  def qsortAndCountMedian[a <% Ordered[a]](xs: Stream[a]) = qsortAndCount(xs)(ys => FuncSort.medianOfThree(ys))
}

object FuncSort {
  val rng = new Random()
  def trStreamMerge(l: Stream[Int], r: Stream[Int]) = {
    @tailrec
    def mergeIter(l: Stream[Int], r: Stream[Int], acc: Stream[Int]) : Stream[Int] = (l, r) match {
      case (Empty, Empty) => acc.reverse
      case (x#::xs, Empty) => mergeIter(xs, r, x#::acc)
      case (Empty, y#::ys) => mergeIter(l, ys, y#::acc)
      case (x#::xs, y#::ys) => if(x < y) mergeIter(xs, r, x#::acc) else mergeIter(l, ys, y#::acc)
    }
    mergeIter(l, r, Empty)
  }

  def streamMerge(l: Stream[Int], r: Stream[Int]) : Stream[Int] = (l, r) match {
    case (x#::xs, Empty) => l
    case (Empty, y#::ys) => r
    case (x#::xs, y#::ys) => if(x < y) x#::streamMerge(xs, r) else y#::streamMerge(l, ys)
  }

  def listMerge(l: List[Int], r: List[Int]) : List[Int] =  (l, r) match {
    case (Nil, Nil) => Nil
    case (x::xs, Nil) => l
    case (Nil, y::ys) => r
    case (x::xs, y::ys) => if(x < y) x::listMerge(xs, r) else y::listMerge(l, ys)
    }

  // Works in n*log(n) time and uses a shit ton of memory
  def streamSort(xs: Stream[Int]) : Stream[Int] = {
    if(xs.lengthCompare(1) <= 0) xs
    else {
      val m = xs.length / 2
      val (l, r) = xs.splitAt(m)
      streamMerge(streamSort(l), streamSort(r))
    }
  }

  // This one fails with StackOverflow
  def mergeAndCount(l: Stream[Int], r: Stream[Int]) : (Long, Stream[Int]) = (l, r) match {
    case (x#::xs, Empty) => (0, l)
    case (Empty, y#::ys) => (0, r)
    case (x#::xs, y#::ys) => if(x < y) {
      lazy val (i, s) = mergeAndCount(xs, r)
      (i, x#::s)
    } else {
      lazy val (i, s) = mergeAndCount(l, ys)
      (i + l.length, y#::s)
    }
  }

  def trMergeAndCount(l: Stream[Int], r: Stream[Int]) = {
    @tailrec
    def mergeIter(l: Stream[Int], r: Stream[Int], acc: Stream[Int], inv: Long) : (Long, Stream[Int]) = {
      (l, r) match {
        case (Empty, Empty) => (inv, acc.reverse)
        case (x#::xs, Empty) => mergeIter(xs, r, x#::acc, inv)
        case (Empty, y#::ys) => mergeIter(l, ys, y#::acc, inv)
        case (x#::xs, y#::ys) => if(x < y) mergeIter(xs, r, x#::acc, inv) else mergeIter(l, ys, y#::acc, inv + l.length)
      }
    }
    mergeIter(l, r, Empty, 0)
  }

  // Takes n*n time for whatever reason
  def sortAndCount(xs: Stream[Int]) : (Long, Stream[Int]) = {
    if(xs.lengthCompare(1) <= 0) {
      (0, xs)
    } else {
      val (l, r) = xs.splitAt(xs.length / 2)
      val (li, ls) = sortAndCount(l)
      val (ri, rs) = sortAndCount(r)
      val (si, sorted) = trMergeAndCount(ls, rs)
      (li+ri+si, sorted)
    }
  }

  def lazySplit[a](xs: Stream[a]) : (Stream[a], Stream[a]) = {
    if(xs.isEmpty) (xs, xs) else (xs.head #:: lazySplit(xs.tail)._2, lazySplit(xs.tail)._1)
  }

  private def qsort[a <% Ordered[a]](xs: Stream[a])(choosePivot:Stream[a] => a): Stream[a] = {
    if(xs.lengthCompare(1) <= 0) xs
    else {
      val pivot = choosePivot(xs)
      val l = xs.filter(_ < pivot)
      val r = xs.filter(_ > pivot)
      qsort(l)(choosePivot) ++ pivot#::qsort(r)(choosePivot)
    }
  }

  private def qsortAndCount[a <% Ordered[a]](xs: Stream[a])(choosePivot:Stream[a] => a): (Int, Stream[a]) = {
    if(xs.lengthCompare(1) <= 0) (0, xs)
    else {
      val pivot = choosePivot(xs)
      val l = xs.filter(_ < pivot)
      val r = xs.filter(_ > pivot)
      val (cl, ls) = qsortAndCount(l)(choosePivot)
      val (cr, rs) = qsortAndCount(r)(choosePivot)
      (xs.length - 1 + cl + cr, ls ++ pivot#::rs)
    }
  }

  def qsortAndCountHead[a <% Ordered[a]](xs: Stream[a]) = qsortAndCount(xs)(_.head)

  def qsortAndCountLast[a <% Ordered[a]](xs: Stream[a]) = qsortAndCount(xs)(_.last)

  def qsortAndCountMedian[a <% Ordered[a]](xs: Stream[a]) = qsortAndCount(xs)(ys => medianOfThree(ys))

  def qsortAndCountRandom[a <% Ordered[a]](xs: Stream[a]) = qsortAndCount(xs)(ys => ys(rng.nextInt(ys.length)))

  def qsortHead[a <% Ordered[a]](xs: Stream[a]) = qsort(xs)(_.head)

  def qsortLast[a <% Ordered[a]](xs: Stream[a]) = qsort(xs)(_.last)

  def qsortRandom[a <% Ordered[a]](xs: Stream[a]) = qsort(xs)(ys => ys(rng.nextInt(ys.length)))

  def qsortMedian[a <% Ordered[a]](xs: Stream[a]) = qsort(xs)(ys => medianOfThree(xs))

  def medianOfThree[a <% Ordered[a]](xs: Stream[a]) = {
    val a = xs.head; val b = xs.last; val c = xs(xs.length / 2)
    if((a < b && b < c) || (c < b && b < a)) b
    else if((a < c && c < b) || (b < c && c < a)) c else a
  }

  def partitionBy[a <% Ordered[a]](xs: Stream[a], pivot: a): (Stream[a], Stream[a]) = {
    @tailrec
    def _partitionBy(ys: Stream[a], lt: Stream[a], gt: Stream[a], pivot: a): (Stream[a], Stream[a]) = ys match {
      case Empty => (lt, gt)
      case h#::t => if(h < pivot) _partitionBy(t, h#::lt, gt, pivot) else if(h > pivot) _partitionBy(t, lt, h#::gt, pivot) else _partitionBy(t, lt, gt, pivot)
    }
    _partitionBy(xs, Empty, Empty, pivot)
  }
}
