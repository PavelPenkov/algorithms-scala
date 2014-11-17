import scala.io._

class TravellingSalesman private (val cities: Array[(Double, Double)]) {
  val n = cities.length

  private val distanceCache = Array.fill(n, n) { Double.NaN }

  val bo = BitSetOps(n)

  def dist(i: Int, j: Int) = {
    if (i == j) 0.0
    else {
      if (distanceCache(i)(j).isNaN) {
        distanceCache(i)(j) = math.sqrt(math.pow(cities(i)._1, 2) + math.pow(cities(i)._2, 2))
      }
      distanceCache(i)(j)
    }
  }

  def routeLength = {
    val solutions = scala.collection.mutable.HashMap[BitSetOps#BitVector, Array[Double]]()

    def get(s: BitSetOps#BitVector, i: Int) = {
      if (i == 1) {
        if (s == 1l) 0.0 else Double.PositiveInfinity
      } else {
        solutions(s)(i)
      }
    }

    def put(s: BitSetOps#BitVector, i: Int, distance: Double) = {
      if (!solutions.contains(s)) {
        solutions(s) = Array.ofDim[Double](n)
      }
      solutions(s)(i) = distance
    }
  }
}

object TravellingSalesman {
  def fromFile(filename: String) = {
    val src = Source.fromFile(filename).getLines()
    val cities = src.map { line =>
      line split "\\s+" match {
        case Array(x, y) => (x.toDouble, y.toDouble)
      }
    }.toArray

    new TravellingSalesman(cities)
  }
}

