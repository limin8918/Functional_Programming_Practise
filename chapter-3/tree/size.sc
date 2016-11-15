sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(l) => f(l)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(l) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def sizeFold[A](t: Tree[A]): Int = {
    fold(t)((l: A) => 1)((l: Int, r: Int) => l + r + 1)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(l) => l
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def maximumFold(t: Tree[Int]): Int = {
    fold(t)((l: Int) => l)((l: Int, r: Int) => l max r)
  }

  def depth(t: Tree[Int]): Int = t match {
    case Leaf(l) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def depthFold(t: Tree[Int]): Int = {
    fold(t)((l: Int) => 0)((l: Int, r: Int) => 1 + (l max r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(l) => Leaf(f(l))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)((l: A) => Leaf(f(l)): Tree[B])((l: Tree[B], r: Tree[B]) => Branch(l, r))
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
Tree.sizeFold(leaf1)
Tree.size(branch12)
Tree.sizeFold(branch12)
Tree.size(branch1)
Tree.sizeFold(branch1)

Tree.maximum(leaf1)
Tree.maximumFold(leaf1)
Tree.maximum(branch12)
Tree.maximumFold(branch12)
Tree.maximum(branch1)
Tree.maximumFold(branch1)

Tree.depth(leaf1)
Tree.depthFold(leaf1)
Tree.depth(branch12)
Tree.depthFold(branch12)
Tree.depth(branch1)
Tree.depthFold(branch1)

Tree.map(leaf1)(x => x.toDouble * 2)
Tree.mapFold(leaf1)(x => x.toDouble * 2)
Tree.map(branch12)(x => x.toDouble * 2)
Tree.mapFold(branch12)(x => x.toDouble * 2)
Tree.map(branch1)(x => x.toDouble * 2)
Tree.mapFold(branch1)(x => x.toDouble * 2)