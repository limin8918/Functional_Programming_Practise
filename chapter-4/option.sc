case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  def getOrElse[EE >: E,B >: A](default: B): B =
    this match {
      case Left(_) => default
      case Right(a) => a
    }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(a => b.map(b => f(a, b)))

  def map2ViaFor[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield (f(aa, bb))
}

def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
  as match {
    case Nil => Right(Nil)
    case h::t => f(h).map2(traverse(t)(f))( (x: B, y: List[B] ) => x::y )
  }

def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
  traverse(es)(a => a)
}


Left(1).map(a => a)
Right(1).map(a => a*2)

Left(1).flatMap(a => Left(2.0))
Right(1).flatMap(a => Right(a*2))

Left(1).orElse(Left(2))
Right(1).orElse(Left(2))

Left(1).map2(Left(2))((l1: Int, l2: Int) => l1 + l2)
Left(1).map2ViaFor(Left(2))((l1: Int, l2: Int) => l1 + l2)
Right(1).map2(Right(2))(_+_)
Right(1).map2ViaFor(Right(2))(_+_)

traverse(List(1, 2, 3))((x: Int) => Right( x + 1 ))
traverse(List(Right(1), Left(2), Right(3)))((x: Either[Int, Int]) => Left("something went wrong"))
traverse(Nil)((x: Int) => Right( x + 1 ))

sequence(List(Right(1), Right(2), Right(3)))
sequence(List(Right(1), Left(2), Right(3)))
