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

  def add1(as: Chain[Int]): Chain[Int] = as match {
    case Nil => Chain[Int]()
    case Cons(head, tail) => Cons((head + 1), add1(tail))
  }

  def add1Right(as: Chain[Int]): Chain[Int] = {
//    foldRight(as, Chain[Int]())((head: Int, tail: Chain[Int]) => Cons(head + 1, tail))
    foldRight(as, Chain[Int]())((head, tail) => Cons(head + 1, tail))
  }
}

Chain.add1(Nil)
Chain.add1Right(Nil)
Chain.add1(Chain(1, 2, 3))
Chain.add1Right(Chain(1, 2, 3))