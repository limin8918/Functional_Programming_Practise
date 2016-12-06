trait RNG 

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = f(rng)
    g(a)(r1)
  }

}


