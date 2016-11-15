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

  def accLength[A](acc: Int, as: Chain[A]): Int = as match {
    case Nil => acc
    case Cons(head, tail) => accLength(acc + 1, tail)
  }
  //  acc + 1即为foldLeft接收的方法

  def LengthTwo[A](as: Chain[A]): Int = accLength(0, as)

  def lengthRight[A](as: Chain[A]): Int = {
    foldRight(as, 0)((head: A, tail: Int) => 1 + tail)
  }

  def lengthLeft[A](as: Chain[A]): Int = {
    foldLeft(as, 0)((x: Int, y: A) => 1 + x)
  }
}

Chain.length(Chain())
Chain.lengthRight(Chain())
Chain.lengthLeft(Chain())
Chain.LengthTwo(Chain())
Chain.length(Chain(1, 2, 3))
Chain.lengthRight(Chain(1, 2, 3))
Chain.lengthLeft(Chain(1, 2, 3))
Chain.LengthTwo(Chain(1, 2, 3))