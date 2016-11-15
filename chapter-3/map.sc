sealed trait Chain[+A]
case object Nil extends Chain[Nothing]
case class Cons[A](head: A, tail: Chain[A]) extends Chain[A]

object Chain {
  def apply[A](as: A*): Chain[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def foldRight[A, B](chain: Chain[A], r: B)(f: (A, B) => B): B = chain match {
    case Nil => r
    case Cons(head, tail) => f(head, foldRight(tail, r)(f))
  }

  def map[A,B](as: Chain[A])(f: A => B): Chain[B] = as match {
    case Nil => Chain[B]()
    case Cons(head, tail) => Cons[B](f(head), map(tail)(f))
  }

  def mapRight[A,B](as: Chain[A])(f: A => B): Chain[B] = {
    foldRight(as, Chain[B]())((head, tail) => Cons[B](f(head), tail))
  }
}

Chain.map(Chain(1, 2, 3))(item => (item + 1).toString)
Chain.mapRight(Chain(1, 2, 3))(item => (item + 1).toString)