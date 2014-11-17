class BitSetOps(val n: Int) {
  type BitVector = Long

  def ofCardinality(cardinality: Int): List[BitVector] = neighborN(0, 0, cardinality)

  def toggled(x: BitVector, i: Int) = x ^ (1 << i)

  def neighborN(x: BitVector, start: Int, dist: Int): List[BitVector] = {
    var result = List.empty[BitVector]
    if(dist == 0) {
      result = x::result
    } else {
      for(i <- start until n; neighbor <- neighborN(toggled(x, i), i+1, dist-1)) {
        result = neighbor::result
      }
    }
    result
  }

  // A bit set of given cardinality that always has 0-th and i-th bits set
  def forTsp(cardinality: Int, i: Int): List[BitVector] = {
    var result = List.empty[BitVector]

    if (cardinality > n || cardinality < 2) throw new IllegalArgumentException(s"Invalid cardinality $cardinality")
    if (i > n-1 || i <1) throw new IllegalArgumentException(s"Invalid bit $i")

    def contains(x: BitVector, i: Int) = (x & (1 << i)) > 0

    for(temp <- neighborN(0, 0, cardinality - 2)) {
      if(!(contains(temp, 0) || contains(temp, i))) {
        result = (temp | 1 | 1 << i)::result
      }
    }
    result
  }
}

object BitSetOps {
  def apply(n: Int) = new BitSetOps(n)
}
