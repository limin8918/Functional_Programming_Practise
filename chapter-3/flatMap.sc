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

  def mapRight[A,B](as: Chain[A])(f: A => B): Chain[B] = {
    foldRight(as, Chain[B]())((head, tail) => Cons[B](f(head), tail))
  }

  def flatMap[A,B](as: Chain[A])(f: A => Chain[B]): Chain[B] = {
    flatten(mapRight(as)(f))
  }

  def filter[A](as: Chain[A])(f: A => Boolean): Chain[A] = {
    flatMap(as)(head => if(f(head)) Chain(head) else Nil)
  }
}

Chain.filter(Chain(1, 2, 3))(item => item > 1)