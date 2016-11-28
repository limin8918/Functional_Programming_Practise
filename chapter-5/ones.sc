case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

sealed trait Stream[+A] {
  import Stream._

  def toList: List[A] = this match {
    case Empty => List[A]()
    case Cons(h, t) => h()::t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
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

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((h, t)) => cons(h, unfold(t)(f))
    }
  }

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] = {
      cons(f0, go(f1, (f0+f1)))
    }
    go(0, 1)
  }
}

Stream.ones.take(5).toList
Stream.constant('a').take(5).toList
Stream.from(10).take(5).toList
Stream.fibs.take(10).toList


def f(s: Stream[Int]) : Option[(String, Stream[Int])] = {
  s match {
    case Empty => None
    case Cons(h, t) => {
      if(h() > 1)
        Some(((h()*5).toString, t()))
      else
        Some("error", t())
    }
  }
}
Stream.unfold(Stream(1, 2, 3))(f).toList