package Monads

// import cats.syntax.MonadOps
// import cats.implicits._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import scala.util.Success
import scala.util.Failure

trait Monad[M[_]] {
  def pure[A](a: A): M[A]
  def bind[A, B](ma: M[A])(f: A => M[B]): M[B]
  def map[A, B](ma: M[A])(f: A => B): M[B] = bind(ma)((a: A) => pure(f(a)))
}

object Monad {
  def apply[M[_]](implicit instance: Monad[M]): Monad[M] = instance

  implicit val OptionMonad: Monad[Option] = new Monad[Option] {
    def pure[A](a: A) = Some(a)
    def bind[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma match
      case Some(a) => f(a)
      case None    => None
  }

  implicit val ListMonad: Monad[List] = new Monad[List] {
    def join[A](ls: List[List[A]]): List[A] =
      ls.foldRight(Nil)((l: List[A], acc: List[A]) => l ++ acc)
    def pure[A](a: A) = List(a)
    def bind[A, B](as: List[A])(f: A => List[B]): List[B] = as match
      case Nil => Nil
      case as  => join(as.map(f))
  }

  // TODO Figure this out
  // type EitherA[A] = Either[A,_]
  // implicit def EitherMonad: Monad[EitherA] = new Monad[EitherA] {
  //   def pure[A](a: A) = Right(a)
  //   def bind[A,B](mab: EitherA[A])(f: B => Either[A,B]): Either[A,B] = mab match
  //     case Left(a) => Left(a)
  //     case Right(b) => f(b)
  // }
}

// object MonadSyntax {
//   implicit class MonadOps[M](value: M):
//     def >>=[A,B](f: A => M[B])(implicit instance: Monad[M]) =
//       instance.bind(value)(f)
//
// }

object Monading {
  import cats.Id
  import cats.implicits._

  def sumSquares[F[_]: cats.Monad](a: F[Int], b: F[Int]): F[Int] = for {
    x <- a
    y <- b
  } yield x + y

  println(Monad[List].bind(List(1, 2, 3))(List(_, 2)))
  println(Monad[List].map(List(1, 2, 3))(_ + 1))
  println(Monad[Option].map(Some(2))(_ + 2))
  println(Monad[Option].map(None)((x: Int) => x + 2))
  println(Monading.sumSquares(Option(3), Option(4)))
  println(Monading.sumSquares(Id(2), Id(3)))
  println(CountPositives.countPositive(List(1, 2, 3, 4)))
  println(CountPositives.countPositive(List(1, 2, 3, -1, 4)))
  println(CountPositives.countPositive2(List(1, 2, 3, 4)))
  println(CountPositives.countPositive2(List(1, 2, 3, -1, 4)))

}

object SecretIdentity {
  type Id[A] = A

  implicit val IdMonad: Monad[Id] = new Monad[Id] {
    def pure[A](a: A): Id[A] = a
    def bind[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma)
  }
}

object IdentityMonad {
  import SecretIdentity._
  import cats.Id

  println(Monad[Id].pure(3))
  println(Monad[Id].bind(3)(_ + 2))
}

object CountPositives {
  import cats._
  import cats.implicits._

  def countPositive(ns: List[Int]): Either[String, Int] =
    ns.foldLeft(0.asRight[String]) { (acc, n) =>
      if n < 0
      then Left("Negative, stopping")
      else acc.map(_ + 1)
    }

  def countPositive2(ns: List[Int]): Option[Int] =
    ns.foldLeft(Option(0)) { (acc, n) =>
      if n < 0
      then None
      else acc.map(_ + 1)
    }
}

object EitherStuff {
  import cats._
  import cats.implicits._
  import scala.util.Try

  // Using either to handle exceptions
  println(Either.catchOnly[NumberFormatException]("foo".toInt))
  println(Either.catchNonFatal(sys.error("bad")))

  // Converting to either
  println(Either.fromTry(Try(sys.error("bad"))))
  println(Either.fromOption(None, "bad"))

  // Transforming either
  println("Error".asLeft[Int].getOrElse(0))
  println("Error".asLeft[Int].orElse(2.asRight[String]))
  println(-1.asRight[String].ensure("Must be non-negative!")(_ > 0))

  // Error handling
  println("Error".asLeft[Int].recover { case _: String =>
    -1
  })
  println("Error".asLeft[Int].recoverWith { case _: String =>
    Right(-1)
  })
}

object Errors {
  import scala.util.Try
  import cats._
  import cats.implicits._

  def validateAdult[F[_]](
      age: Int
  )(implicit me: MonadError[F, Throwable]): F[Int] =
    if age >= 18
    then me.pure(age)
    // then age.pure[F]
    else me.raiseError(IllegalArgumentException("Age must be over 18"))
    // else IllegalArgumentException("Age must be over 18").raiseError[F, Int]

  def validateAdult2[F[_]: MonadThrow](age: Int): F[Int] =
    if age >= 18
    then age.pure[F]
    else IllegalArgumentException("Age must be over 18").raiseError[F, Int]

  println(validateAdult[Try](18))
  println(validateAdult[Try](8))

  type ExceptionOr[A] = Either[Throwable, A]
  println(validateAdult[ExceptionOr](-1))
}

object EvalMonad {
  // Call-by-value
  val x = {
    println("ahoy")
    2
  }

  // Call-by-name
  def y = {
    println("ahoy")
    2
  }

  // Call-by-need
  lazy val z = {
    println("ahoy")
    2
  }

  import cats.Eval

  // You can use the `value` field to extract their value
  val now = Eval.now(math.random + 1000)
  val always = Eval.always(math.random + 3000)
  val later = Eval.later(math.random + 2000)

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
    as match {
      case head :: tail =>
        foldRight(tail, acc)(fn).map(fn(head, _))
      case Nil =>
        Eval.now(acc)
    }
}

object WriterMonad {
  import cats.data.Writer
  import cats._
  import cats.implicits._

  println(
    Writer(
      Vector(
        "One",
        "Two"
      ),
      1859
    )
  )

  type Logged[A] = Writer[Vector[String], A]

  // Needs a monoid instance in scope
  println(123.pure[Logged])

  def sajt: Writer[List[Int], Int] = for {
    _ <- List(1, 2, 3).tell
    _ <- List(3, 4, 5).tell
    c <- 2.writer(List(3, 4, 5))
  } yield c

  def sajt2: Writer[List[Int], Int] =
    List(1, 2, 3).tell.flatMap(_ => List(3, 4, 5).tell.map(_ => 2))

  val writer1: Logged[Int] = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b

  // Transform the value in context
  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))

  // Transform both
  val writer3 = writer1.bimap(
    log => log.map(_.toUpperCase),
    res => res * 100
  )

  val writer4 = writer1.mapBoth { (log, res) =>
    val log2 = log.map(_ + "!")
    val res2 = res * 1000
    (log2, res2)
  }

  // Change value in context to identity value
  val writer5 = writer1.reset

  // Swap values
  val writer6 = writer1.swap

  println(writer1.run)
  println(writer1.written)
  println(writer1.value)

  def slowly[A](body: => A) =
    try body
    finally Thread.sleep(100)

  def factorial(n: Int): Logged[Int] = for {
    ans <-
      if n == 0
      then 1.pure[Logged]
      else slowly(factorial(n - 1).map(_ * n))
    _ <- Vector(s"fact $n $ans").tell
  } yield ans

  // println(factorial(5).run)
  import scala.concurrent._
  import scala.concurrent.duration._

  val res = Await.result(
    Future
      .sequence(
        Vector(
          Future(factorial(5)),
          Future(factorial(5))
        )
      )
      .map(_.map(_.written)),
    5.seconds
  )

  println(res)
}

object ReaderMonad {
  import cats.data.Reader
  import cats._
  import cats.implicits._

  case class Config(catNum: Int, cats: List[String])
  case class Cat(name: String, food: String)

  def sajt: Reader[Int, Int] = for {
    a <- Reader[Int, Int](identity)
  } yield a

  val f: Reader[Config, String] =
    Reader[Config, String](conf => s"${conf.cats}")
  val g: Reader[Config, Int] = Reader[Config, Int](conf => conf.catNum)

  def combineThem: Reader[Config, String] = for {
    catNames <- f
    catNum <- g
  } yield s"$catNames and $catNum"

  val catName: Reader[Cat, String] = Reader(c => c.name)
  val catGreet: Reader[Cat, String] =
    catName.map(name => s"Have a nice day, $name!")
  val catFeed: Reader[Cat, String] =
    Reader(c => s"Here is a nice bowl of ${c.food}!")

  val catGreetAndFeed: Reader[Cat, String] = for {
    greet <- catGreet
    feed <- catFeed
  } yield s"$greet $feed"

  println(sajt.map(_ + 1).run(3))
  println(combineThem.map(_.toUpperCase).run(Config(2, List("Fritzy", "Fred"))))
  // These are equivalent
  println(catGreetAndFeed(Cat("Jozsef", "Almaspite")))
  println(catGreetAndFeed.run(Cat("Jozsef", "Almaspite")))
}

object ReaderLoginSystem {
  import cats.data.Reader
  import cats._
  import cats.implicits._

  case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))
    // Using the error-throwing API
    // Reader(db =>
    //   Try(db.usernames(userId)) match
    //     case Success(value) => Some(value)
    //     case Failure(_)     => None
    // )

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = for {
    username <- findUsername(userId)
    result <- username
      .map { username =>
        checkPassword(username, password)
      }
      .getOrElse(false.pure[DbReader])
    // result <- username match
    //   case Some(user) => checkPassword(user, password)
    //   case None       => false.pure[DbReader]
  } yield result

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  println(checkLogin(1, "zerocool").run(db))
  println(checkLogin(4, "davinci").run(db))
}

object StateMonad {
  import cats._
  import cats.implicits._
  import cats.data.State

  val a: State[Int, String] =
    State(n => (n, s"The state is $n"))

  // The result is wrapped inside an Eval for stack-safety
  println(a.run(2).value)
  println(a.runS(2).value)
  println(a.runA(2).value)

  // Each State instance represents a single state transition
  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step 1 is $ans")
  }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step 2 is $ans")
  }

  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)

  val getting = State.get[Int]
  val setting = State.set[Int](20)
  val puring = State.pure[Int, String]("Value")
  val inspecting = State.inspect[Int, String](s => s"$s")
  val modifying = State.modify[Int](s => s + 1)

  val program: State[Int, String] = for {
    a <- State.get[Int]
    _ <- State.modify[Int](_ * 2)
    b <- State.get[Int]
    _ <- State.modify[Int](_ + 2)
    c <- State.get[Int]
  } yield s"$a then $b then $c"

  println(both.run(3).value)
  println(program.run(3).value)
}

object StateCalculator {
  import cats._
  import cats.implicits._
  import cats.data.State

  case class Stack[A](s: List[A]) {
    def isEmpty: Boolean = s.isEmpty

    def push(a: A) = Stack(a :: s)

    def pop: Option[(Stack[A], A)] =
      if isEmpty
      then None
      else Some((Stack(s.tail), s.head))

    def peek: Option[A] = if isEmpty then None else Some(s.head)
  }

  object Stack {
    def apply[A]: Stack[A] = Stack(List[A]())
  }

  enum Term:
    case Plus
    case Minus
    case Mult
    case Div
    case Num(n: Int)
    case CalcError
  import Term.*

  def eval(operator: Term)(x: Term, y: Term): Term = (operator, x, y) match
    case (Plus, Num(x), Num(y))  => Num(y + x)
    case (Minus, Num(x), Num(y)) => Num(y - x)
    case (Mult, Num(x), Num(y))  => Num(y * x)
    case (Div, Num(x), Num(y))   => Num(y / x)
    case _                       => CalcError

  def applyOperator(op: (Term, Term) => Term): State[Stack[Term], Unit] =
    State.modify { s =>
      (s.pop >>= ((newStack, op1) =>
        newStack.pop >>= ((newStack, op2) => newStack.push(op(op1, op2)).pure)
      )).getOrElse(Stack[Term](List(CalcError)))
    }

  def calculator(terms: List[Term]): State[Stack[Term], Term] = terms match
    case Nil => State.get.map(_.peek.getOrElse(CalcError))
    case (t :: ts) =>
      t match
        case Plus  => applyOperator(eval(Plus)) >> calculator(ts)
        case Minus => applyOperator(eval(Minus)) >> calculator(ts)
        case Mult  => applyOperator(eval(Mult)) >> calculator(ts)
        case Div   => applyOperator(eval(Div)) >> calculator(ts)
        case Num(n: Int) =>
          State.modify[Stack[Term]](s => s.push(Num(n))) >> calculator(ts)
        case CalcError => throw Error("Shouldn't happen")

  println(
    calculator(
      // List(Num(3), Num(2), Minus, Num(2), Plus)
      List(Num(1), Num(2), Plus, Num(3), Mult)
    ).runA(Stack[Term]).value
  )
}

object BetterStateCalculator {
  import cats._
  import cats.implicits._
  import cats.data.State

  type CalcState[A] = State[List[Int], A]

  def operand(n: Int): CalcState[Int] =
    State[List[Int], Int] { stack => (n :: stack, n) }

  def operator(op: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: t =>
        val ans = op(a, b)
        (ans :: t, ans)

      case _ => sys.error("Oh no!")
    }

  def evalOne(sym: String): CalcState[Int] = sym match
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case n   => operand(n.toInt)

  println(evalOne("42").runA(Nil).value)

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    _ <- evalOne("+")
    _ <- evalOne("1")
    ans <- evalOne("+")
  } yield ans

  // def evalAll(input: List[String]): CalcState[Int] = input match
  //   case Nil     => State[List[Int], Int] { case res :: Nil => (Nil, res) }
  //   case s :: ss => evalOne(s) >> evalAll(ss)

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState])(_ >> evalOne(_))

  def evalInput(input: String) =
    evalAll(input.split(" ").toList).runA(Nil).value

  println(program.runA(Nil).value)
  println(evalAll(List("1", "2", "+", "3", "*")).runA(Nil).value)
  println(evalInput("1 2 + 3 *"))
}

object Equivalent {
  import cats.data.State

  def f(n: Int): State[List[Int], Int] = for
    v <- State.get
    _ <- State.set(n :: v)
  yield n

  def g(n: Int): State[List[Int], Int] =
    State.get.flatMap(v => State.set(n :: v).map(_ => n))

  def h(n: Int): State[List[Int], Int] =
    State({ state => (n :: state, n) })
}

object OptionMonad {
  import cats.Monad
  import scala.annotation.tailrec

  val optionMonad = new cats.Monad[Option] {
    def flatMap[A, B](opt: Option[A])(fn: A => Option[B]): Option[B] = opt match
      case None    => None
      case Some(a) => fn(a)

    def pure[A](a: A): Option[A] = Some(a)

    @tailrec
    def tailRecM[A, B](a: A)(fn: A => Option[Either[A, B]]): Option[B] =
      fn(a) match {
        case None           => None
        case Some(Left(a1)) => tailRecM(a1)(fn)
        case Some(Right(b)) => Some(b)
      }
  }
}

object TreeMonad {
  import cats._
  import cats.syntax.flatMap._
  // import cats.implicits._

  sealed trait Tree[+A]
  final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](a: A) extends Tree[A]

  object Tree {
    implicit val TreeMonad: cats.Monad[Tree] = new cats.Monad[Tree] {
      def pure[A](a: A): Tree[A] = Leaf(a)

      def flatMap[A,B](t: Tree[A])(f: A => Tree[B]): Tree[B] = t match
        case Node(l,r) => Node(l.flatMap(f), r.flatMap(f))
        case Leaf(a)   => f(a)

      // This is the non-tail-recursive solution!
      def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
        f(a).flatMap {
          case Left(a)  => tailRecM(a)(f)
          case Right(b) => Leaf(b)
        }
    }
  }

  println(cats.Monad[Tree].flatMap(Node(Leaf(1),Leaf(2)))(a => Node(Leaf(a), Leaf(a * 2))))
   
}

@main
def main: Unit =
  // EitherStuff
  // Errors
  // EvalMonad
  // WriterMonad
  // ReaderMonad
  // ReaderLoginSystem
  // StateMonad
  // StateCalculator
  // BetterStateCalculator
  TreeMonad
