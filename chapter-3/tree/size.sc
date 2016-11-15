sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(l) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }
}

Tree.size(Leaf(1))
Tree.size(Branch(Leaf(1), Leaf(2)))
Tree.size(Branch(Leaf(1), Branch(Leaf(3), Leaf(4))))