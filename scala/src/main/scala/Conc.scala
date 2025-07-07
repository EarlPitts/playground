def sum(ints: Seq[Int]): Int =
  ints.foldRight(0)(_ + _)

def sumDC(ints: IndexedSeq[Int]): Int =
  if ints.size <= 1
  then ints.headOption.getOrElse(0)
  else
    val (l, r) = ints.splitAt(ints.size / 2)
    sumDC(l) + sumDC(r)

// def unit[A](a: => A): Par[A] = ???
//
// def get[A](a: Par[A]): A = ???

// def sumPar(ints: IndexedSeq[Int]): Par[Int] =
//   if ints.size <= 1
//   then Par.unit(ints.headOption.getOrElse(0))
//   else
//     val (l,r) = ints.splitAt(ints.size / 2)
//     Par.map2(sumPar(l), sumPar(r))(_ + _)
