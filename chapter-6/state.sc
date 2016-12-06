trait RNG

object RNG {

//  type Rand[+A] = RNG => (A, RNG)

  type State[S, +A] = S => (A, S)

  type Rand[A] = State[RNG, A]
}