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

  def foldLeft[A, B](chain: Chain[A], r: B)(f: (B, A) => B): B = chain match {
    case Nil => r
    case Cons(head, tail) => foldLeft(tail, f(r, head))(f)
  }

  def sum(intChain: Chain[Int]): Int = intChain match {
    case Nil => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def accSum(acc: Int, intChain: Chain[Int]): Int = intChain match {
    case Nil => acc
    case Cons(head, tail) => accSum(head + acc, tail)
  }
  //  head + acc即为foldLeft接收的方法

  def sumTwo(intChain: Chain[Int]): Int = accSum(0, intChain)

  def sumRight(intChain: Chain[Int]): Int = {
    foldRight(intChain, 0)(_ + _)
  }

  def sumLeft(intChain: Chain[Int]): Int = {
    foldLeft(intChain, 0)(_ + _)
  }

  def product(doubleChain: Chain[Double]): Double = doubleChain match {
    case Nil => 1.0
    case Cons(head, tail) => head * product(tail)
  }

  def accProduct(acc: Double, doubleChain: Chain[Double]): Double = doubleChain match {
    case Nil => acc
    case Cons(head, tail) => accProduct(head * acc, tail)
  }
  //  head * acc即为foldLeft接收的方法

  def productTwo(doubleChain: Chain[Double]): Double = {
    accProduct(1.0, doubleChain)
  }

  def productRight(doubleChain: Chain[Double]): Double = {
    foldRight(doubleChain, 1.0)(_ * _)
  }

  def productLeft(doubleChain: Chain[Double]): Double = {
    foldLeft(doubleChain, 1.0)(_ * _)
  }
}

Chain.sum(Chain())
Chain.sumRight(Chain())
Chain.sumLeft(Chain())
Chain.sumTwo(Chain())
Chain.sum(Chain(1, 2, 3))
Chain.sumRight(Chain(1, 2, 3))
Chain.sumLeft(Chain(1, 2, 3))
Chain.sumTwo(Chain(1, 2, 3))
Chain.product(Chain())
Chain.productRight(Chain())
Chain.productLeft(Chain())
Chain.productTwo(Chain())
Chain.product(Chain(1, 2, 3))
Chain.productRight(Chain(1, 2, 3))
Chain.productLeft(Chain(1, 2, 3))
Chain.productTwo(Chain(1, 2, 3))