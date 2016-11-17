case object None extends Option[Nothing]
case class Some[A](get: A) extends Option[A]

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def flatMap_2[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse(None)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(a) => Some(a)
  }

  def orElse_2[B >: A](ob: => Option[B]): Option[B] = {
    map(x => Some(x)) getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) => if(f(a)) this else None
    case None => None
  }

  def filter_2(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  def filter_3(f: A => Boolean): Option[A] = {
    flatMap(
      (x: A) => {
        if (f(x))
          Some(x)
        else
          None
      }
    )
  }
}

None.map((x: Int) => x.toString)
Some(3).map((x: Int) => x.toString)

None.getOrElse(0)
Some(3).getOrElse(0)

None.flatMap((x: Int) => Some(x.toString + "abc"))
None.flatMap_2((x: Int) => Some(x.toString + "abc"))
Some(3).flatMap((x: Int) => Some(x.toString + "abc"))
Some(3).flatMap_2((x: Int) => Some(x.toString + "abc"))

None.orElse(None)
None.orElse_2(None)
Some(3).orElse(None)
Some(3).orElse_2(None)

None.filter((x: Int) => x > 0)
None.filter_2((x: Int) => x > 0)
None.filter_3((x: Int) => x > 0)
Some(0).filter((x: Int) => x > 0)
Some(0).filter_2((x: Int) => x > 0)
Some(0).filter_3((x: Int) => x > 0)
Some(3).filter((x: Int) => x > 0)
Some(3).filter_2((x: Int) => x > 0)
Some(3).filter_3((x: Int) => x > 0)

