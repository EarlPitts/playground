object Intro:

  // Comment
  /* Another
  * comment */
  /** Docstring */

  // Pattern matching
  def sajt(n: Int) =
    n match
      case 0 => "zero"
      case 1 => "one"
      case 2 => "two"
      case _ => "other"

  def fact(n: Int): Int =
    n match
      case 0 => 1
      case n => n * fact(n-1)

  def factTail(n: Int): Int =
    def go(n: Int, acc: Int): Int =
        n match
          case 0 => acc
          case _ => go(n-1, acc * n)
    go(n,1)

  def fibo(n: Int): Int =
    n match
      case 0 => 1
      case 1 => 1
      case _ => fibo(n-1) + fibo(n-2)

  def fiboTail(n: Int): Int =
    def go(n: Int, acc1: Int, acc2: Int): Int =
      n match
        case 0 => acc1
        case 1 => acc2
        case _ => go(n-1, acc2, acc1 + acc2)
    go(n, 1, 1)

  val x = (1 to 10).map(fact(_))
  val y = (1 to 10).map(fibo(_))
  val z = (1 to 10).map(fiboTail(_))

  // This object stuff can be omitted, Scala suuports top-level defs
  // Just think of this as a module
  object MyProgram:

    def abs(n: Int): Int =
      if n > 0 then n else (-n)

    private def formatResult(name: String, f: Int => Int, n: Int): String =
      val msg = "The %s of %d is %d."
      msg.format(name, n, f(n))

    // Return types can be omitted
    private def formatAbs(x: Int) =
      val msg = "The absolute value of %d is %d."
      msg.format(x, abs(x))

    private def formatFactorial(n: Int) =
      val msg = "The factorial of %d is %d."
      msg.format(n, fact(n))

    // For nullary procedures, we write parens, but not for functions
    // This annotation signals that this is the entry point
    @main def printAbsAndFactorial(): Unit =
      println(formatAbs(-42))
      println(formatFactorial(7))
      println(formatResult("absolute value",abs,-7))

  def find[A](xs: List[A], x: A): Option[A] =
    xs match
      case Nil => None
      case s :: rest => if (x == s) then Some(s) else find(rest, x)

  def findArray[A](xs: Array[A], x: A): Option[Int] =
    def go(n: Int): Option[Int] =
      if n >= xs.length then None
      else if xs(n) == x then Some(n) else go(n+1)
    go(0)

  def findPred[A](xs: List[A], p: A => Boolean): Option[A] =
    xs match
      case Nil       => None
      case x :: rest => if p(x) then Some(x) else findPred(rest, p)

  val l = List(1,2,3,4,5,6,7,8,9,10)
  val array = Array(7,9,12,13)

  val a = findPred(l, (x) => x % 2 == 0)

  def id[A](x: A): A = x

  val newL = l.flatMap((x) => List(x,x+1))

  // Currying
  def f(x:Int)(y:Int): Int = x + y

  def isSorted[A](l: List[A], f:(A, A) => Boolean): Boolean =
    l match
      case Nil => true
      case _ :: Nil => true
      case x :: y :: ls => f(x,y) && isSorted(y::ls, f)

  object Combinators:
    def partial1[A,B,C](a: A, f: (A, B) => C): B => C =
      (b) => f(a,b)

    val p = partial1(1, (x: Int, y: Int) => x + y)
    val _ = p(2) // results in 3

    def curry[A,B,C](f: (A,B) => C): A => B => C =
      a => (b => f(a,b))

    def uncurry[A,B,C](f: A => B => C): (A,B) => C =
      (a,b) => f(a)(b)

    def flip[A,B,C](f: (A,B) => C): (B,A) => C =
      (b,a) => f(a,b)

    val curriedPlus = curry((x: Int,y: Int) => x + y)
    val plus = uncurry(curriedPlus)

    def comp[A,B,C](f: B => C, g: A => B): A => C =
      (x: A) => f(g(x))

    // Composition from stdlib
    def f1(x: Int): Int = x + 1
    def g1(x: Int): Int = x - 1
    val same1 = f1 compose g1
    val same2 = f1 andThen g1
