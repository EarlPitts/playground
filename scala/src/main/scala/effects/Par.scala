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

object ParTypeclass extends IOApp.Simple:
  // parMapN (and all functions with the "par" prefix) converts
  // the sequential IO values to IO.Par values, runs them in parallel, then
  // translates back to IO
  val a = (IO.pure(2), IO.pure(3)).parMapN(_ + _)

  def run: IO[Unit] = for
    res <- a
    _ <- IO.println(res)
  yield ()

object DebugExample extends IOApp.Simple:
  import debug.*

  val hello = IO("hello").myDebug
  val world = IO("world").myDebug

  def run: IO[Unit] =
    (hello, world)
      // mapN will run on the smae thread sequentially
      // .mapN((h, w) => s"$h $w")
      // parMapN runs on different threads concurrently
      .parMapN((h, w) => s"$h $w")
      .myDebug
      .as(IO.unit)

object ParMapNErrors extends IOApp.Simple:
  import debug.*

  // val ok = (IO.sleep(1.second) >> IO("hi")).myDebug
  val ok = IO("hi").myDebug
  val ko1 = IO.raiseError[String](new RuntimeException("oh!")).myDebug
  val ko2 = IO.raiseError[String](new RuntimeException("noes!")).myDebug

  // val e1 = (ok, ko1).parMapN((_, _) => ())
  val e1 = (ok, ko1).parTupled.void
  val e2 = (ko1, ok).parMapN((_, _) => ())
  val e3 = (ko1, ko2).parMapN((_, _) => ())

  def run: IO[Unit] =
    e1.attempt.myDebug >>
      IO("---").myDebug >>
      e2.attempt.myDebug >>
      IO("---").myDebug >>
      e3.attempt.myDebug.void

object parTraverse extends IOApp.Simple:
  val l = (1 to 100).toList

  def run: IO[Unit] = for
    _ <- l.parTraverse(IO.println(_))
    _ <- l.map(IO.println(_)).parSequence
  yield ()
