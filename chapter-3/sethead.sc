sealed trait Chain[+A]
case object Nil extends Chain[Nothing]
case class Cons[A](head: A, tail: Chain[A]) extends Chain[A]

object Chain {
  def apply[A](as: A*): Chain[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def setHead[A](chain: Chain[A], head: A): Chain[A] = chain match {
    case Nil => Chain(head)
    case Cons(a, b) => Cons(head, b)
  }
}

Chain.setHead(Chain(), 1)
Chain.setHead(Chain(1, 2, 3), 2)
Chain.setHead(Chain('a', 'b', 'c'), 'b')