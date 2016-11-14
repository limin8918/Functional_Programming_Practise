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

  def foldLeft[A, B](chain: Chain[A], r: B)(f: (B, A) => B): B = chain match {
    case Nil => r
    case Cons(head, tail) => foldLeft(tail, f(r, head))(f)
  }

  def length[A](as: Chain[A]): Int = as match {
    case Nil => 0
    case Cons(head, tail) => 1 + length(tail)
  }

  def lengthRight[A](as: Chain[A]): Int = {
    foldRight(as, 0)((x: A, y: Int) => 1 + y)
  }

  def lengthLeft[A](as: Chain[A]): Int = {
    foldLeft(as, 0)((x: Int, y: A) => 1 + x)
  }
}

Chain.length(Chain())
Chain.lengthRight(Chain())
Chain.lengthLeft(Chain())
Chain.length(Chain(1, 2, 3))
Chain.lengthRight(Chain(1, 2, 3))
Chain.lengthLeft(Chain(1, 2, 3))