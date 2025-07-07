// import cats.*
import cats.implicits.*
import io.monad.*

object IOMonad:

  trait IO[A]:
    self =>
    def unsafeRun: A
    def map[B](f: A => B): IO[B] = new:
      def unsafeRun = f(self.unsafeRun)
    def flatMap[B](f: A => IO[B]): IO[B] = new:
      def unsafeRun = f(self.unsafeRun).unsafeRun

  object IO:
    import Monad.*

    def apply[A](a: => A): IO[A] = new:
      def unsafeRun = a

    implicit val monad: Monad[IO] = new Monad[IO]:
      def unit[A](a: => A): IO[A] = IO(a)
      extension [A](fa: IO[A])
        override def flatMap[B](f: A => IO[B]) =
          fa.flatMap(f)

import IOMonad.*

def ReadLine: IO[String] = IO(scala.io.StdIn.readLine())
def PrintLine(s: String): IO[Unit] = IO(println(s))

IO(1).map(_ + 2).unsafeRun
IOMonad.IO(2).flatMap((x: Int) => IOMonad.IO(x + 1)).unsafeRun

def greeter: IO[Unit] =
  PrintLine("Enter your name: ")
    .flatMap(_ =>
      ReadLine
        .flatMap(name => PrintLine(s"Hello $name"))
    )

def echo = ReadLine.flatMap(PrintLine)
def readInt = ReadLine.map(_.toInt)

object Factorial:
  import cats.effect.*

  def factorial(n: Int): IO[Int] = for
    acc <- IO.ref(1)
    _ <- (1 to n).toList.traverse(i => acc.update(_ * i).void)
    result <- acc.get
  yield result

import cats.effect.unsafe.implicits.global

Factorial.factorial(5).unsafeRunSync()

// StackOverflow problems
val p = PrintLine("Still going...").forever
// p.unsafeRun
