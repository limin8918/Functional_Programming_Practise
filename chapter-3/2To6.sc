sealed trait Chain[+A]
case object Nil extends Chain[Nothing]
case class Cons[A](head: A, tail: Chain[A]) extends Chain[A]

object Chain {
  def sum(intChain: Chain[Int]): Int = intChain match {
    case Nil => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def product(doubleChain: Chain[Double]): Double = doubleChain match {
    case Nil => 1.0
    case Cons(0.0, tail) => 0.0
    case Cons(head, tail) => head * product(tail)
  }

  def apply[A](as: A*): Chain[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](chain: Chain[A]): Chain[A] = chain match {
    case Nil => Nil
    case Cons(a, b) => b
  }

  def setHead[A](chain: Chain[A], head: A): Chain[A] = chain match {
    case Nil => Chain(head)
    case Cons(a, b) => Cons(head, b)
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
      if(f(a)) Cons(a, dropWhile(tail(chain), f))
      else dropWhile(tail(chain), f)
    }
  }

  def init[A](chain: Chain[A]): Chain[A] = chain match {
    case Nil => Nil
    case Cons(a, Nil) => Nil
    case Cons(a, b) => Cons(a, init(b))
  }
}

Chain.tail(Chain())
Chain.tail(Chain(1, 2, 3))
Chain.tail(Chain('a', 'b', 'c'))

Chain.setHead(Chain(), 1)
Chain.setHead(Chain(1, 2, 3), 2)
Chain.setHead(Chain('a', 'b', 'c'), 'b')

Chain.drop(Nil, 1)
Chain.drop(Chain(1), 0)
Chain.drop(Chain(1, 2, 3), 2)
Chain.drop(Chain(1, 2, 3), 4)

Chain.dropWhile(Nil, (x: Int) => x > 1)
Chain.dropWhile(Chain(1, 2, 3), (x: Int) => x > 4)
Chain.dropWhile(Chain(1, 2, 3), (x: Int) => x > 0)
Chain.dropWhile(Chain(1, 2, 3), (x: Int) => x > 1)
Chain.dropWhile(Chain(3, 1, 4), (x: Int) => x > 1)

Chain.init(Nil)
Chain.init(Chain(1))
Chain.init(Chain(1, 2))
Chain.init(Chain(1, 2, 3))