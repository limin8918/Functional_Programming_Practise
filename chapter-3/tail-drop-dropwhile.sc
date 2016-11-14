sealed trait Chain[+A]
case object Nil extends Chain[Nothing]
case class Cons[A](head: A, tail: Chain[A]) extends Chain[A]

object Chain {
  def apply[A](as: A*): Chain[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](chain: Chain[A]): Chain[A] = chain match {
    case Nil => Nil
    case Cons(a, b) => b
  }

  def drop[A](chain: Chain[A], n: Int): Chain[A] = {
      if (n == 0) Nil
      else {
        chain match {
          case Nil => Nil
          case Cons(a, b) => Cons(a, drop(tail(chain), n - 1))
        }
      }
  }

  def dropWhile[A](chain: Chain[A], f: A => Boolean): Chain[A] = chain match {
    case Nil => Nil
    case Cons(a, b) => {
      if(f(a)) dropWhile(tail(chain), f)
      else Cons(a, dropWhile(tail(chain), f))
    }
  }
}

Chain.tail(Chain())
Chain.tail(Chain(1.0, 2.0, 3.0))
Chain.tail(Chain('a', 'b', 'c'))
Chain.drop(Nil, 1)
Chain.drop(Chain(1), 0)
Chain.drop(Chain(1, 2, 3), 2)
Chain.drop(Chain(1, 2, 3), 4)
Chain.dropWhile(Nil, (x: Int) => x > 1)
Chain.dropWhile(Chain(1, 2, 3), (x: Int) => x > 4)
Chain.dropWhile(Chain(1, 2, 3), (x: Int) => x > 0)
Chain.dropWhile(Chain(3, 1, 2), (x: Int) => x > 1)
