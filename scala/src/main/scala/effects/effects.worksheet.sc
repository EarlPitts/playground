import cats.effect.*
import cats.*
import cats.implicits.*

object Zero:
  class PrintLine(d: => String):
    def unsafeRun: Unit = println(d)

  case class Player(name: String, score: Int)

  def contest(p1: Player, p2: Player): PrintLine =
    PrintLine(winnerMsg(winner(p1, p2)))

  def winner(p1: Player, p2: Player): Option[Player] =
    if p1.score > p2.score then Some(p1)
    else if p2.score > p1.score then Some(p2)
    else None

  def winnerMsg(p: Option[Player]): String =
    p.map { case Player(_, name) => s"${name} is the winner!" }
      .getOrElse("It's a draw.")

val p8 = Zero.Player("jani", 14)
val p9 = Zero.Player("bela", 13)

Zero.contest(p8, p9).unsafeRun

object One:
  // This will only form a monoid
  trait IO:
    self =>
    def unsafeRun: Unit
    def ++(io: IO): IO = new:
      def unsafeRun =
        self.unsafeRun
        io.unsafeRun

  object IO:
    def empty: IO = new:
      def unsafeRun = ()

  case class Player(name: String, score: Int)

  def PrintLine(msg: String): IO = new:
    def unsafeRun = println(msg)

  def winner(p1: Player, p2: Player): Option[Player] =
    if p1.score > p2.score then Some(p1)
    else if p2.score > p1.score then Some(p2)
    else None

  def winnerMsg(p: Option[Player]): String = p
    .map:
      case Player(name, _) => s"$name is the winner!"
    .getOrElse("It's a draw.")

  def contest(p1: Player, p2: Player): IO =
    PrintLine(winnerMsg(winner(p1, p2)))

  def contest2(p1: Player, p2: Player): IO =
    PrintLine(winnerMsg(winner(p1, p2))) ++
      PrintLine(winnerMsg(winner(p1, p2)))

val p1 = One.Player("jani", 123)
val p2 = One.Player("pali", 100)
One.contest(p1, p2).unsafeRun

object Two:
  trait Monad[F[_]]:
    def unit[A](a: => A): F[A]
    extension [A](fa: F[A]) def flatMap[B](f: A => F[B]): F[B]

  trait IO[A]:
    self =>
    def unsafeRun: A
    def map[B](f: A => B): IO[B] = new:
      def unsafeRun = f(self.unsafeRun)
    def flatMap[B](f: A => IO[B]): IO[B] = new:
      def unsafeRun = f(self.unsafeRun).unsafeRun

  object IO:
    def apply[A](a: => A): IO[A] = new:
      def unsafeRun = a

    given monad: Monad[IO] with
      def unit[A](a: => A): IO[A] = IO(a)
      extension [A](fa: IO[A])
        override def flatMap[B](f: A => IO[B]) =
          fa.flatMap(f)

  case class Player(name: String, score: Int)

  // def PrintLine(msg: String): IO[Unit] = new:
  //   def unsafeRun = println(msg)
  //
  // def ReadLine: IO[String] = new:
  //   def unsafeRun = scala.io.StdIn.readLine

  def PrintLine(msg: String): IO[Unit] =
    IO(println(msg))

  def ReadLine: IO[String] =
    IO(scala.io.StdIn.readLine)

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  def converter: IO[Unit] = for
    _ <- PrintLine("Enter a temperature in degree Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  yield ()

  def winner(p1: Player, p2: Player): Option[Player] =
    if p1.score > p2.score then Some(p1)
    else if p2.score > p1.score then Some(p2)
    else None

  def winnerMsg(p: Option[Player]): String = p
    .map:
      case Player(name, _) => s"$name is the winner!"
    .getOrElse("It's a draw.")

  def contest(p1: Player, p2: Player): IO[Unit] =
    PrintLine(winnerMsg(winner(p1, p2)))

  def contest2(p1: Player, p2: Player): IO[Unit] =
    PrintLine("sajt").flatMap(_ => PrintLine(winnerMsg(winner(p1, p2))))

  val echo: IO[Unit] = ReadLine.flatMap(PrintLine)
  val readInt: IO[Int] = ReadLine.map(_.toInt)

val p3 = Two.Player("jani", 123)
val p4 = Two.Player("pali", 100)
Two.contest(p3, p4).unsafeRun

val ios = List(2, 3, 4)
ios.traverse(IO.println(_))
ios.map(IO.println(_)).sequence
