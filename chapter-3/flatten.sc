sealed trait Chain[+A]
case object Nil extends Chain[Nothing]
case class Cons[A](head: A, tail: Chain[A]) extends Chain[A]

object Chain {
  def apply[A](as: A*): Chain[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def foldRight[A, B](l: Chain[A], r: B)(f: (A, B) => B): B = l match {
    case Nil => r
    case Cons(head, tail) => f(head, foldRight(tail, r)(f))
  }

  def append[A](l: Chain[A], r: Chain[A]): Chain[A] = l match {
    case Nil => r
    case Cons(head, tail) => Cons(head, append(tail, r))
  }

  def flatten[A](as: Chain[Chain[A]]): Chain[A] = {
    foldRight(as, Nil:Chain[A])(append)
  }
}

Chain.flatten(Chain(Chain()))
Chain.flatten(Chain(Chain(1), Chain(2), Chain(3)))