case object None extends Option[Nothing]
case class Some[A](get: A) extends Option[A]

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(a) => Some(a)
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) => if(f(a)) this else None
    case None => None
  }

  def filter_2(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}

None.map((x: Int) => x.toString)
Some(3).map((x: Int) => x.toString)

None.flatMap((x: Int) => Some(x.toString + "abc"))
Some(3).flatMap((x: Int) => Some(x.toString + "abc"))

None.getOrElse(0)
Some(3).getOrElse(0)

None.orElse(None)
Some(3).orElse(None)

None.filter((x: Int) => x > 0)
None.filter_2((x: Int) => x > 0)
Some(0).filter((x: Int) => x > 0)
Some(0).filter_2((x: Int) => x > 0)
Some(3).filter((x: Int) => x > 0)
Some(3).filter_2((x: Int) => x > 0)



