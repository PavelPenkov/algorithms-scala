package me.penkov.utils

object Benchmark {
  def realtime[A](f: => A) = {
    val start = System.currentTimeMillis
    val result = f
    (System.currentTimeMillis() - start, result)
  }

  def realtime(f: => Unit) = {
    val start = System.currentTimeMillis
    f
    System.currentTimeMillis() - start
  }

  def bm(message: String)(f: => Unit) = {
    val start = System.currentTimeMillis
    f
    println(s"$message ${System.currentTimeMillis() - start}")
  }
}
