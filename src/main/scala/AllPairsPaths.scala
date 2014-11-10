object AllPairsPaths extends App {
  def process(filename: String): Unit = {
    val wg = WeightedDigraph.fromFile(filename)
    println(s"Processing $filename with ${wg.n} vertices and ${wg.m} edges")
    wg.floydWarshal match {
      case Left(v) => println(s"$filename has negative cycle containing $v")
      case Right(paths) => {
        val shortestPath = (for(i <- 0 until wg.n; j <- 0 until wg.n if i != j) yield paths(i)(j)).min
        println(shortestPath)
      }
    }
  }

  for(i <- 1 to 3) {
    process(s"g$i.txt")
  }
}
