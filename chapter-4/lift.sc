case object None extends Option[Nothing]
case class Some[A](get: A) extends Option[A]

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
}

None.map((x: Int) => x.toString)
Some(3).map((x: Int) => x.toString)

def lift[A, B](f: A => B): (Option[A] => Option[B]) =
  (x: Option[A]) => x.map(f)
def lift2[A, B](f: A => B): Option[A] => Option[B] = _ map f

lift((x: Int) => x.toString)(None)
lift2((x: Int) => x.toString)(None)
lift((x: Int) => x.toString)(Some(3))
lift2((x: Int) => x.toString)(Some(3))