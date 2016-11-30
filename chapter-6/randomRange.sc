trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    if(i < 0) (-i, r)
    else (i, r)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0)
      (List(), rng)
    else {
      val (i, r1) = nonNegativeInt(rng)
      val (l, r2) = ints(count - 1)(r1)
      (i::l, r2)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }
}

val rng =  SimpleRNG(10)
val (n1, rng1) = rng.nextInt
val (n2, _) = rng.nextInt
val (n3, _) = rng1.nextInt

val (n4, _) = RNG.nonNegativeInt(rng)
val (n5, _) = RNG.nonNegativeInt(rng)
val (n6, _) = RNG.nonNegativeInt(rng1)

val (n7, _) = RNG.double(rng)
val (n8, _) = RNG.double(rng)
val (n9, _) = RNG.double(rng1)

val (n10, _) = RNG.ints(0)(rng)
val (n11, _) = RNG.ints(3)(rng)