object Utils {
  def default[a] = {
    class DefaultVal {
      var x: a = _
    }
    (new DefaultVal).x
  }

  def minMax(x: Int, y: Int) = (math.min(x,y), math.max(x,y))

  implicit def intWithTimes(n: Int) = new {
    def times(f: => Unit) = 1 to n foreach { _ => f }
  }

  implicit def arrayWithTransform[a](xs: Array[a]) = new {
    def transform(i: Int)(f: a => a):Unit  = xs(i) = f(xs(i))
  }
}
