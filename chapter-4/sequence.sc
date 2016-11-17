sealed trait Chain[+A]
case object Nil extends Chain[Nothing]
case class Cons[A](head: A, tail: Chain[A]) extends Chain[A]

object Chain {
  def apply[A](as: A*): Chain[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }
}
case object None extends Option[Nothing]
case class Some[A](get: A) extends Option[A]


def sequence[A](a: Chain[Option[A]]): Option[Chain[A]] = a match {
  case Nil => Some(Nil)
  case Cons(h, t) => h.flatMap(hh => sequence(t).map(Cons(hh, _)))
}

sequence(Nil)
sequence(Chain(Some(3), Some(4)))