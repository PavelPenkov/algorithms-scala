object TheoryProblems {
  def unimodalMax[a <% Ordered[a]](xs: Array[a]): a = {
    if(xs.length == 2) {
      if(xs(0) < xs(1)) xs(1) else xs(0)
    }
    else {
      val m = xs.length/2
      if(xs(m-1) < xs(m)) unimodalMax(xs.drop(m)) else unimodalMax(xs.take(m))
    }
  }
}
