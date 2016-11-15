sealed trait Chain[+A]
case object Nil extends Chain[Nothing]
case class Cons[A](head: A, tail: Chain[A]) extends Chain[A]

object Chain {
  def apply[A](as: A*): Chain[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def foldRight[A, B](chain: Chain[A], r: B)(f: (A, B) => B): B = chain match {
    case Nil => r
    case Cons(head, tail) => f(head, foldRight(tail, r)(f))
  }

  def filter[A](as: Chain[A])(f: A => Boolean): Chain[A] = as match {
    case Nil => Nil
    case Cons(head, tail) => {
      if(f(head)) Cons(head, filter(tail)(f)) else tail
    }
  }

  def filterRight[A](as: Chain[A])(f: A => Boolean): Chain[A] = {
    foldRight(as, Nil: Chain[A])((head, tail) => {
      if(f(head)) Cons(head, tail) else tail
    })
  }
}

Chain.filter(Chain(1, 2, 3))(item => item > 1)
Chain.filterRight(Chain(1, 2, 3))(item => item > 1)