trait RNG

object RNG {

//  type Rand[+A] = RNG => (A, RNG)

  type State[S, +A] = S => (A, S)

  type Rand[A] = State[RNG, A]

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = f(rng)
    g(a)(r1)
  }

//  def modify[S](f: S => S): State[S, Unit] = for {
//    s <- get
//    _ <- set(f(s))
//  } yield ()
//
//  def get[S]: State[S, S] = { (s: S) =>
//    (s, s)
//  }
//
//  def set[S](s: S): State[S, Unit] = { s2 =>
//    ((), s)
//  }
}

// We might want to write it as its own class
// Wrapping the underlying function like this
case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s) )
//  def set[S](s: S): State[S, Unit] = State(s2 => ((), s) )
}

def modifyTheState(x: Int): State[Int, Unit] = for {
  _ <- State.set(x + 1) // Has type State[A, Unit] - sets the state.
// Note that this example is not how you would do it normally, you would actually
// use State.modify - it is over complicated to be a good example.
} yield (())
modifyTheState(10).run(5)