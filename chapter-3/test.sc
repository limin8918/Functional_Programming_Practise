{
  sealed trait Chain
  case object Nil extends Chain
  case class Cons(head: Int, tail: Chain) extends Chain

  object Chain {
    def sum(intChain: Chain): Int = intChain match {
      case Nil => 0
      case Cons(head, tail) => head + sum(tail)
    }
  }
  println(Chain.sum(Cons(5, Nil)))
  println(Chain.sum(Cons(5, Cons(4, Nil))))
}

{
  sealed trait Chain[+A]
  case object Nil extends Chain[Nothing]
  case class Cons[A](head: A, tail: Chain[A]) extends Chain[A]

  object Chain {
    def sum(intChain: Chain[Int]): Int = intChain match {
      case Nil => 0
      case Cons(head, tail) => head + sum(tail)
    }

    def product(doubleChain: Chain[Double]): Double = doubleChain match {
      case Nil => 1.0
      case Cons(0.0, tail) => 0.0
      case Cons(head, tail) => head * product(tail)
    }
  }
  println(Chain.sum(Cons(5, Cons(4, Nil))))
  println(Chain.product(Cons(5.0, Cons(4.0, Nil))))

  val ex1: Chain[Double] = Nil
  val ex2: Chain[Int] = Nil
}