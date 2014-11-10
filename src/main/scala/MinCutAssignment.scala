object MinCutAssignment extends App {
  import scala.io.Source

  val graph = Graph(Source.fromFile(args(0)))

  val n = 100
  val sz = graph.minCutSize(n)

  println(s"After $n iterations size is $sz")
}
