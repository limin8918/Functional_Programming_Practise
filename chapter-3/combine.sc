sealed trait Chain[+A]
case object Nil extends Chain[Nothing]
case class Cons[A](head: A, tail: Chain[A]) extends Chain[A]

object Chain {
  def apply[A](as: A*): Chain[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def combine(a: Chain[Int], b: Chain[Int]): Chain[Int] = (a, b) match {
    case (Nil, Nil) => Nil
    case (Nil, head) => Nil
    case (head, Nil) => Nil
    case (Cons(head1, tail1), Cons(head2, tail2)) => {
      Cons((head1 + head2), combine(tail1, tail2))
    }
  }

  def zipWith[A, B, C](a: Chain[A], b: Chain[B])(f: (A, B) => C): Chain[C] =  (a,b) match {
    case (Nil, _) => Nil: Chain[C]
    case (_, Nil) => Nil: Chain[C]
    case (Cons(head1, tail1), Cons(head2, tail2)) => {
      Cons(f(head1, head2), zipWith(tail1, tail2)(f))
    }
  }
}
Chain.combine(Chain(), Chain())
Chain.combine(Chain(1), Chain())
Chain.combine(Chain(), Chain(2))
Chain.combine(Chain(1, 2, 3), Chain(1, 2))

Chain.zipWith(Nil: Chain[Int], Chain[Double]())((x, y) => x.toString + y.toString)
Chain.zipWith(Chain(1), Nil: Chain[Int])((x, y) => x.toString + y.toString)
Chain.zipWith(Nil: Chain[Int], Chain(1.0))((x, y) => x.toString + y.toString)
Chain.zipWith(Chain(1, 2, 3), Chain(1.0, 2.0))((x, y) => x.toString + y.toString)
Chain.zipWith(Chain("aa", "bb"), Chain("cc", "dd", "ee"))((x, y) => x + y)