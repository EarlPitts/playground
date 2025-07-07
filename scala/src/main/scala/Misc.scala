lazy val fib: LazyList[Int] = 0 #:: 1 #:: fib.zip(fib.tail).map((x,y) => x + y)
