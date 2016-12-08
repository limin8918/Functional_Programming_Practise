def sum(ints: IndexedSeq[Int]): Int = {
  if(ints.length <= 1)
    ints.headOption getOrElse(0)
  else {
    val (l, r) = ints.splitAt(ints.length/2)
    sum(l) + sum(r)
  }
}
sum(IndexedSeq(1, 2, 3, 4))