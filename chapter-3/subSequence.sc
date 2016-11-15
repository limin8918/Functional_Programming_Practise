sealed trait Chain[+A]
case object Nil extends Chain[Nothing]
case class Cons[A](head: A, tail: Chain[A]) extends Chain[A]

object Chain {
  def apply[A](as: A*): Chain[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def hasSubsequence[A](sup: Chain[A], sub: Chain[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(head1, tail1), Cons(head2, tail2)) => {
      if(head1 == head2)
        startsWith(tail1, tail2)
      else
        hasSubsequence(tail1, sub)
    }
  }

  def startsWith[A](l: Chain[A], prefix: Chain[A]): Boolean = (l,prefix) match {
    case (_, Nil) => true
    case (Cons(h,t), Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  def hasSubsequenceTwo[A](sup: Chain[A], sub: Chain[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_,t) => hasSubsequenceTwo(t, sub)
  }
}

println(1)
Chain.hasSubsequence(Nil, Nil)
Chain.hasSubsequenceTwo(Nil, Nil)
println(2)
Chain.hasSubsequence(Chain(1), Nil)
Chain.hasSubsequenceTwo(Chain(1), Nil)
println(3)
Chain.hasSubsequence(Nil, Chain(1))
Chain.hasSubsequenceTwo(Nil, Chain(1))
println(4)
Chain.hasSubsequence(Chain(1), Chain(1))
Chain.hasSubsequenceTwo(Chain(1), Chain(1))
println(5)
Chain.hasSubsequence(Chain(1), Chain(2))
Chain.hasSubsequenceTwo(Chain(1), Chain(2))
println(6)
Chain.hasSubsequence(Chain(1, 2, 3, 4), Chain(4))
Chain.hasSubsequenceTwo(Chain(1, 2, 3, 4), Chain(4))
println(7)
Chain.hasSubsequence(Chain(1, 2, 3, 4), Chain(4, 3))
Chain.hasSubsequenceTwo(Chain(1, 2, 3, 4), Chain(4, 3))
println(8)
Chain.hasSubsequence(Chain(1, 2, 3, 4), Chain(1, 3))
Chain.hasSubsequenceTwo(Chain(1, 2, 3, 4), Chain(1, 3))
println(9)
Chain.hasSubsequence(Chain(1, 2, 3, 4), Chain(2, 3, 4))
Chain.hasSubsequenceTwo(Chain(1, 2, 3, 4), Chain(2, 3, 4))
