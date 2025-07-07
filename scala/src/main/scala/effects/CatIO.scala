package effects

import cats.effect.{IO, IOApp, ExitCode, Resource, Sync}
import cats.effect.unsafe.implicits._
import cats.effect.implicits._
import scala.concurrent.duration._
import scala.concurrent._
import scala.io.Source
import cats.syntax.all._
import cats.effect.kernel.Deferred
import java.util.concurrent.Executors
import scala.util.Success
import scala.util.Failure
import java.util.concurrent.CompletableFuture
import java.io.RandomAccessFile

object debug {
  /** Extension methods for an effect of type `F[A]`. */
  implicit class DebugHelper[A](ioa: IO[A]) {

    /** Print to the console the value of the effect along with the thread it
      * was computed on.
      */
    def myDebug: IO[A] =
      for {
        a <- ioa
        tn = Thread.currentThread.getName
        _ = println(s"[${tn}] $a") // <1>
      } yield a
  }
}

object TickingClock extends IOApp.Simple:
  def run: IO[Unit] = tickingClock

  def tickingClock: IO[Unit] =
    IO.println(System.currentTimeMillis)
      >> IO.sleep(1.second)
      >> tickingClock

object Future1 extends App:
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  // val hello = Future(println(s"[${Thread.currentThread.getName}] Hello"))
  // val world = Future(println(s"[${Thread.currentThread.getName}] World"))
  // val -> def delays execution
  def hello = Future(println(s"[${Thread.currentThread.getName}] Hello"))
  def world = Future(println(s"[${Thread.currentThread.getName}] World"))

  // val hw1: Future[Unit] = for
  //   _ <- hello
  //   _ <- world
  // yield ()

  val hw1: Future[Unit] = hello.flatMap(_ => world.flatMap(_ => Future(())))

  Await.ready(hw1, 1.seconds) // This timeouts for some reason

  val hw2: Future[Unit] =
    (hello, world).mapN((_, _) => ())

  Await.ready(hw2, 1.seconds)

object IOComp extends App:
  val hello = IO(println(s"[${Thread.currentThread.getName}] Hello"))
  val world = IO(println(s"[${Thread.currentThread.getName}] World"))

  // val hw1: IO[Unit] = for
  //   _ <- hello
  //   _ <- world
  // yield ()

  // val hw1: IO[Unit] = (hello >> world).void // This hangs, no idea why

  val hw2: IO[Unit] =
    (hello, world).parMapN((_, _) => ())

  // hw1.unsafeRunSync()
  hw2.unsafeRunSync()

object Example1 extends IOApp.Simple:
  def repeat(letter: String): IO[Unit] =
    IO.print(letter).replicateA(100).void

  def run: IO[Unit] = for
    fa <- (repeat("A") *> repeat("B")).as("foo!").start
    fb <- (repeat("C") *> repeat("D")).as("bar!").start
    ra <- fa.joinWithNever
    rb <- fb.joinWithNever
    _ <- IO.println(s"\ndone: a says: $ra, b says: $rb")
  yield ()

object Example2 extends IOApp.Simple:
  def run: IO[Unit] = for
    fiber <- (IO.println("hello!") >> IO.sleep(1.seconds)).foreverM.start
    _ <- IO.sleep(5.seconds)
    _ <- fiber.cancel
  yield ()

object Example3 extends IOApp.Simple:
  val a = IO.println("hello!").foreverM
  val b = IO.println("world!").replicateA_(10)
  // b should always win, as a runs forever

  def run: IO[Unit] = for
    c <- IO.race(a, b)
    _ <- (c match
      case Left(_)  => IO.println("a won")
      case Right(_) => IO.println("b won")
    )
  yield ()

object Example4 extends IOApp.Simple:
  def fact(n: Long): Long = if (n == 0) then 1 else n * fact(n - 1)

  def run: IO[Unit] = for
    res <- IO.race(IO(fact(20)), IO(fact(20)))
    _ <- res.fold(
      a => IO.println(s"Right hand side won: $a"),
      b => IO.println(s"Left hand side won: $b")
    )
  yield ()

