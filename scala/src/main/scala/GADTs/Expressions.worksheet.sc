object Untyped:
  sealed trait Expr
  case class Lit(n: Int) extends Expr
  case class Add(n: Expr, m: Expr) extends Expr
  case class Mul(n: Expr, m: Expr) extends Expr
  case class IsZero(n: Expr) extends Expr

  sealed trait Result
  case class IntResult(n: Int) extends Result
  case class BoolResult(b: Boolean) extends Result

  def eval(e: Expr): Option[Result] = e match
    case Lit(n) => Some(IntResult(n))
    case Add(n, m) => (eval(n), eval(m)) match
      case (Some(IntResult(n)), Some(IntResult(m))) => Some(IntResult(n + m))
      case _ => None
    case Mul(n, m) => (eval(n), eval(m)) match
      case (Some(IntResult(n)), Some(IntResult(m))) => Some(IntResult(n * m))
      case _ => None
    case IsZero(n) => eval(n) match
      case Some(IntResult(n)) => Some(BoolResult(n == 0))
      case _ => None

  // Scala also has union types, but we lose some type guarantees compared to tagged unions
  // def eval(e: Expr): Option[Int | Boolean] = e match
  //   case Lit(n) => Some(n)
  //   case Add(n, m) =>
  //     (eval(n), eval(m)) match
  //       case (Some(n: Int), Some(m: Int)) => Some(n + m)
  //       case _                            => None
  //   case Mul(n, m) =>
  //     (eval(n), eval(m)) match
  //       case (Some(n: Int), Some(m: Int)) => Some(n * m)
  //       case _                            => None
  //   case IsZero(n) =>
  //     eval(n) match
  //       case Some(n: Int) => Some(n == 0)
  //       case _            => None

// import Untyped.*

// val expr1: Expr = IsZero(Add(Lit(2), Lit(3)))
// val expr2: Expr = Add(Lit(2), IsZero(Lit(3)))

object PhantomTyped:
  type Term[T] = Expr

  sealed trait Expr
  case class Lit(n: Int) extends Expr
  case class Add(n: Expr, m: Expr) extends Expr
  case class Mul(n: Expr, m: Expr) extends Expr
  case class IsZero(n: Expr) extends Expr

  sealed trait Result
  case class IntResult(n: Int) extends Result
  case class BoolResult(b: Boolean) extends Result

  // Functions that wrap our constructors ("smarter" constructors)
  def lit(n: Int): Term[Int] = Lit(n)
  def add(t1: Term[Int], t2: Term[Int]): Term[Int] = Add(t1, t2)
  def mul(t1: Term[Int], t2: Term[Int]): Term[Int] = Mul(t1, t2)
  def isZero(n: Term[Int]): Term[Boolean] = IsZero(n)

  // The evaluator still have to check, because there is no explicit connection
  // between Term and Expr, even though we can only construct valid expressions
  def eval(e: Expr): Option[Result] = e match
    case Lit(n) => Some(IntResult(n))
    case Add(n, m) =>
      (eval(n), eval(m)) match
        case (Some(IntResult(n)), Some(IntResult(m))) => Some(IntResult(n + m))
        case _                                        => None
    case Mul(n, m) =>
      (eval(n), eval(m)) match
        case (Some(IntResult(n)), Some(IntResult(m))) => Some(IntResult(n * m))
        case _                                        => None
    case IsZero(n) =>
      eval(n) match
        case Some(IntResult(n)) => Some(BoolResult(n == 0))
        case _                  => None

  // We also have to wrap the evaluator, and use the right one
  def int_eval(t: Term[Int]): Option[Int] = eval(t) match
    case Some(IntResult(n)) => Some(n)
    case _                  => None

  def bool_eval(t: Term[Boolean]): Option[Boolean] = eval(t) match
    case Some(BoolResult(b)) => Some(b)
    case _                   => None

// import PhantomTyped.*

// Scala unfortunately drops the added type tag :(
// val expr1 = add(lit(2), isZero(lit(3))) // Shouldn't work
// val expr2: Expr = Add(Lit(2), Lit(3))

object Typed:
  sealed trait Expr[T]
  case class Lit(n: Int) extends Expr[Int]
  case class Add(n: Expr[Int], m: Expr[Int]) extends Expr[Int]
  case class Mul(n: Expr[Int], m: Expr[Int]) extends Expr[Int]
  case class IsZero(n: Expr[Int]) extends Expr[Boolean]

  def eval[A](e: Expr[A]): A = e match
    case Lit(n)    => n
    case Add(n, m) => eval(n) + eval(m)
    case Mul(n, m) => eval(n) * eval(m)
    case IsZero(n) => eval(n) == 0

// import Typed.*

// val expr1 = Add(Lit(2), Lit(3))
// val expr2 = IsZero(Add(Lit(2), Lit(3)))
// val expr3 = Add(IsZero(Lit(2)), Lit(3))

// eval(expr2)
