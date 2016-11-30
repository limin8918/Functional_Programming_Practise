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
}

val rng =  SimpleRNG(1)
val (n1, rng1) = rng.nextInt
val (n2, _) = rng.nextInt
val (n3, _) = rng1.nextInt

val (n4, _) = RNG.nonNegativeInt(rng)
val (n5, _) = RNG.nonNegativeInt(rng)
val (n6, _) = RNG.nonNegativeInt(rng1)

