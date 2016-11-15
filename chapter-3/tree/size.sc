sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(l) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(l) => l
    case Branch(left, right) => maximum(left) max maximum(right)
  }
}

val leaf1 = Leaf(1)
val leaf2 = Leaf(2)
val leaf3 = Leaf(3)
val leaf4 = Leaf(4)

val branch12 = Branch(leaf1, leaf2)
val branch34 = Branch(leaf3, leaf4)
val branch1 = Branch(leaf1, branch34)

Tree.size(leaf1)
Tree.size(branch12)
Tree.size(branch1)

Tree.maximum(leaf1)
Tree.maximum(branch12)
Tree.maximum(branch1)