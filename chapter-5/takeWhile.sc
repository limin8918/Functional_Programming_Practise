case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => List[A]()
    case Cons(h, t) => h()::t().toList
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => {
      if(p(h()))
        Cons(h, () => t().takeWhile(p))
      else
        t().takeWhile(p)
    }
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

Stream().takeWhile((x: Int) => x > 1).toList
Stream(1, 2, 3).takeWhile((x: Int) => x > 1).toList
Stream(1, 2, 3).takeWhile((x: Int) => x > 3).toList

