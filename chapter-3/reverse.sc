sealed trait Chain[+A]
case object Nil extends Chain[Nothing]
case class Cons[A](head: A, tail: Chain[A]) extends Chain[A]

object Chain {
  def apply[A](as: A*): Chain[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def foldLeft[A, B](chain: Chain[A], r: B)(f: (B, A) => B): B = chain match {
    case Nil => r
    case Cons(head, tail) => foldLeft(tail, f(r, head))(f)
  }

  def accReverse[A](acc: Chain[A], add: Chain[A]): Chain[A] = add match {
    case Nil => acc
    case Cons(head, tail) => accReverse(Cons(head, acc), tail)
  }
  //  accReverse即为foldLeft接收的方法

  def reverse[A](as: Chain[A]): Chain[A] = accReverse(Chain[A](), as)

  def reverseLeft[A](as: Chain[A]): Chain[A] = {
    foldLeft(as, Chain[A]())((acc,h) => Cons(h,acc))
  }
}
Chain.reverse(Chain())
Chain.reverseLeft(Chain())
Chain.reverse(Chain(1, 2, 3))
Chain.reverseLeft(Chain(1, 2, 3))