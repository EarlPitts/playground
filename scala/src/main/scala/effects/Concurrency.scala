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

object Conc extends IOApp.Simple:
  import debug.*

  def myParMap2Nope[A, B, C](ia: IO[A], ib: IO[B])(f: (A, B) => C): IO[C] = for
    fiberA <- ia.start
    fiberB <- ib.start
    // This won't work
    a <- fiberA.joinWithNever.onError(_ => fiberB.cancel)
    b <- fiberB.joinWithNever.onError(_ => fiberA.cancel)
  yield f(a, b)

  def myParMap2[A, B, C](ia: IO[A], ib: IO[B])(f: (A, B) => C): IO[C] =
    IO.racePair(ia, ib).flatMap {
      case Left(a, fb)  => (a.embedNever, fb.joinWithNever).mapN(f)
      case Right(fa, b) => (fa.joinWithNever, b.embedNever).mapN(f)
    }

  val joined: IO[String] = for
    fiber <- IO("task").start
    res <- fiber.joinWithNever
  yield res

  val task: IO[String] =
    IO.sleep(1.seconds) >> IO("task").myDebug

  def run: IO[Unit] = for
    // _ <- IO.println("task").start
    // _ <- IO.println(s"${Thread.currentThread.getName} task")
    // _ <- joined.flatMap(IO.println(_))
    fiber <- task.start
    _ <- IO("pre-join").myDebug
    _ <- fiber.joinWithNever.myDebug
    _ <- IO("post-join").myDebug
  yield ()

object Cancel extends IOApp.Simple:
  import debug.*

  // def run: IO[Unit] = for
  //   fiber <- task
  //     .onCancel(IO("I was cancelled").myDebug.void)
  //     .start
  //   _ <- IO("pre-cancel").myDebug >> IO.sleep(1.second)
  //   _ <- fiber.cancel
  //   _ <- IO("cancelled").myDebug
  // yield ()

  def run: IO[Unit] = for _ <- (tickingClock, ohNoes).parTupled
  yield ()

  val task: IO[String] =
    IO("task").myDebug *> IO.never

  val tickingClock: IO[Unit] = for
    _ <- IO(System.currentTimeMillis).myDebug
    _ <- IO.sleep(1.second)
    _ <- tickingClock
  yield ()

  val ohNoes =
    IO.sleep(2.seconds) *> IO.raiseError(new RuntimeException("oh noes!"))

object Timeout extends IOApp.Simple:
  import debug.*

  def run: IO[Unit] = for
    done <- IO.race(task, timeout)
    _ <- done match
      case Left(_)  => IO("   task: won").myDebug
      case Right(_) => IO("timeout: won").myDebug
  yield ()

  val task: IO[Unit] = annotatedSleep("   task", 1000.millis)
  val timeout: IO[Unit] = annotatedSleep("timeout", 500.millis)

  def annotatedSleep(name: String, duration: FiniteDuration): IO[Unit] =
    (
      IO(s"$name: starting").myDebug *>
        IO.sleep(duration) *>
        IO(s"$name: done").myDebug
    ).onCancel(IO(s"$name: cancelled").myDebug.void).void
