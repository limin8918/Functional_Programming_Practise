case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

sealed trait Stream[+A] {
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => {
      if(n > 0)
        Cons(h, () => t().take(n - 1))
      else
        Empty
    }
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => {
      if(n > 0)
        t().drop(n - 1)
      else
        Cons(h, t)
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

Stream().take(0)
Stream().take(1)
Stream(1, 2, 3).take(2)
Stream(1, 2, 3).take(4)

Stream().drop(0)
Stream().drop(1)
Stream(1, 2, 3).drop(0)
Stream(1, 2, 3).drop(1)
Stream(1, 2, 3).drop(3)