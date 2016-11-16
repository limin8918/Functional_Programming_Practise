sealed trait Option[+A]
case object None extends Option[Nothing]
case class Some[A](get: A) extends Option[A]

def mean(xs: Seq[Double]): Option[Double] = {
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)
}

mean(Seq())
mean(Seq(1.0, 2.0))
