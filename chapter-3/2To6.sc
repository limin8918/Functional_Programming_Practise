sealed trait Chain[+A]
case object Nil extends Chain[Nothing]
case class Cons[A](head: A, tail: Chain[A]) extends Chain[A]

object Chain {
  def foldRight[B](chain: Chain[B], r: B)(f: (B, B) => B): B = chain match {
    case Nil => r
    case Cons(head, tail) => f(head, foldRight(tail, r)(f))
  }

  def sum(intChain: Chain[Int]): Int = {
    foldRight(intChain, 0)(_ + _)
  }

  def product(doubleChain: Chain[Double]): Double = {
    foldRight(doubleChain, 1.0)(_ * _)
  }

  def length[A](as: Chain[A]): Int = as match {
    case Nil => 0
    case Cons(head, tail) => 1 + length(tail)
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
      if(f(a)) dropWhile(tail(chain), f)
      else Cons(a, dropWhile(tail(chain), f))
    }
  }

  def init[A](chain: Chain[A]): Chain[A] = chain match {
    case Nil => Nil
    case Cons(a, Nil) => Nil
    case Cons(a, b) => Cons(a, init(b))
  }
}

Chain.sum(Chain())
Chain.sum(Chain(1, 2, 3))
Chain.product(Chain())
Chain.product(Chain(1, 2, 3))
Chain.length(Chain())
Chain.length(Chain(1, 2, 3))
Chain.tail(Chain())
Chain.tail(Chain(1.0, 2.0, 3.0))
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
Chain.dropWhile(Chain(3, 1, 2), (x: Int) => x > 1)
Chain.init(Nil)
Chain.init(Chain(1))
Chain.init(Chain(1, 2))
Chain.init(Chain(1, 2, 3))