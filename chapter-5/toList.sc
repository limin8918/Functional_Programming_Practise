case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Empty => List[A]()
      case Cons(h, t) => h()::t().toList
    }

  def toList2: List[A] = {
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => go(t(), h()::acc)
    }

    go(this, List()).reverse
  }

  def toList3: List[A] = {
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => h()::go(t(), acc)
    }

    go(this, List())
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

Stream().toList
Stream().toList2
Stream().toList3
Stream(1, 2).toList
Stream(1, 2).toList2
Stream(1, 2).toList3