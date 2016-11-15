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
  //  Cons(head, ..)即为foldRight的方法

  def appendRight[A](l: Chain[A], r: Chain[A]): Chain[A] = {
    foldRight(l, r)((head: A, tail: Chain[A]) => Cons(head, tail))
  }
  //  foldRight第一个参数为head, 第二个参数可以理解为对tail进行calculate
}

Chain.append(Chain(1), Nil)
Chain.appendRight(Chain(1), Nil)
Chain.append(Chain(1), Chain(2))
Chain.appendRight(Chain(1), Chain(2))
Chain.append(Chain(1), Chain(2, 3))
Chain.appendRight(Chain(1), Chain(2, 3))

