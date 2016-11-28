case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

sealed trait Stream[+A] {
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def forAllViaFold(p: A => Boolean): Boolean = {
    foldRight(true)((x, b) => p(x) && b)
  }
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

Stream().forAll((x: Int) => x > 1)
Stream().forAllViaFold((x: Int) => x > 1)
Stream(1, 2, 3).forAll((x: Int) => x > 0)
Stream(1, 2, 3).forAllViaFold((x: Int) => x > 0)
Stream(1, 2, 3).forAll((x: Int) => x > 2)
Stream(1, 2, 3).forAllViaFold((x: Int) => x > 2)