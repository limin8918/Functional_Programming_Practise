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

  def dToS(as: Chain[Double]): Chain[String] = as match {
    case Nil => Chain[String]()
    case Cons(head, tail) => Cons(head.toString, dToS(tail))
  }

  def dToSRight(as: Chain[Double]): Chain[String] = {
    foldRight(as, Chain[String]())((head, tail) => Cons[String](head.toString, tail))
  }
}

Chain.dToS(Nil)
Chain.dToSRight(Nil)
Chain.dToS(Chain(1.0, 2.0, 3.0))
Chain.dToSRight(Chain(1.0, 2.0, 3.0))