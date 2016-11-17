sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }
}
case object None extends Option[Nothing]
case class Some[A](get: A) extends Option[A]


def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  a.flatMap( aa => ( b.map( bb => f(aa, bb) ) ) )
}

map2(Some(2), Some(3))((x,y) => x*y)