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

object Parallelism extends IOApp.Simple:
  import debug.*

  def run: IO[Unit] = for
    _ <- IO(s"number of CPSs: $numCpus").myDebug
    _ <- tasks.myDebug
  yield ()

  val numCpus = Runtime.getRuntime().availableProcessors()
  val tasks = List.range(0, numCpus * 2).parTraverse(task)
  def task(i: Int): IO[Int] = IO(i).myDebug

object Blocking extends IOApp.Simple:
  import debug.*

  def tickingClock: IO[Unit] = for
    _ <- IO(System.currentTimeMillis()).myDebug
    _ <- IO.sleep(1.second)
    // _ <- IO.cede // Unnecessary, every bind is an async boundary
    _ <- tickingClock
  yield ()

  def run: IO[Unit] = for
    // _ <- IO("on default threadpool").myDebug
    // _ <- IO.blocking("on blocking threadpool").myDebug
    _ <- (tickingClock, tickingClock, tickingClock, tickingClock).parTupled
  yield ()

object Shifting extends IOApp.Simple:
  import debug.*

  def run: IO[Unit] =
    (ec("1"), ec("2")) match {
      case (ec1, ec2) =>
        for
          _ <- IO("one").myDebug
          // _ <- IO.shift(ec1) // CE2
          _ <- IO("two").myDebug
          // _ <- IO.shift(ec2)
          _ <- IO("three").myDebug
        yield ()
    }

  def ec(name: String): ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor { r =>
      val t = new Thread(r, s"pool-$name-thread-1")
      t.setDaemon(true)
      t
    })

object Async extends IOApp.Simple:
  import debug.*

  trait API:
    def compute: Future[Int] = ???

  def run: IO[Unit] = ???

  def doSomething[A](api: API)(implicit ec: ExecutionContext): IO[Int] =
    IO.async[Int] { cb =>
      IO(Some(IO(api.compute.onComplete {
        case Failure(t) => cb(Left(t))
        case Success(a) => cb(Right(a))
      })))
    }.guarantee(IO.cede)
