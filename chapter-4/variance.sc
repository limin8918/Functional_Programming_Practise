case object None extends Option[Nothing]
case class Some[A](get: A) extends Option[A]

def mean(xs: Seq[Double]): Option[Double] = {
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)
}

sealed trait Option[+A] {
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    ???
  }
}