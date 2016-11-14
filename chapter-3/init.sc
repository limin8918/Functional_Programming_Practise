sealed trait Chain[+A]
case object Nil extends Chain[Nothing]
case class Cons[A](head: A, tail: Chain[A]) extends Chain[A]

object Chain {
  def apply[A](as: A*): Chain[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def init[A](chain: Chain[A]): Chain[A] = chain match {
    case Nil => Nil
    case Cons(a, Nil) => Nil
    case Cons(a, b) => Cons(a, init(b))
  }
}

Chain.init(Nil)
Chain.init(Chain(1))
Chain.init(Chain(1, 2))
Chain.init(Chain(1, 2, 3))