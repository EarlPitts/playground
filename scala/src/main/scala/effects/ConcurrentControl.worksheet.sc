import cats.*
import cats.data.*
import cats.implicits.*
import cats.effect.*

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global

val clock: IO[Long] = IO(System.currentTimeMillis())
extension [A](io: IO[A])

  def t: IO[A] = for
    before <- clock
    res <- io
    after <- clock
    duration = after - before
    _ <- IO(println(s"took $duration"))
  yield res

  def d: IO[A] =
    io.flatTap { a =>
      IO(println(s"[${Thread.currentThread.getName}] $a"))
    }

trait CountdownLatch:
  def await: IO[Unit]
  def decrement: IO[Int]

enum State:
  case Outstanding(n: Int, whenDone: Deferred[IO, Unit])
  case Done
import State.*

object CountdownLatch:
  def make(count: Int): IO[CountdownLatch] = for
    d <- Deferred[IO, Unit]
    state <- Ref[IO].of[State](Outstanding(count, d))

    latch = new CountdownLatch:
      def await: IO[Unit] = state.get.flatMap {
        case Done              => IO.unit
        case Outstanding(_, d) => d.get.void
      }

      def decrement: IO[Int] = state.flatModify {
        case Done => (Done, IO.pure(0))
        case Outstanding(1, d) =>
          (Done, d.complete(()) >> IO.pure(0))
        case Outstanding(count, d) =>
          (Outstanding(count - 1, d), IO.pure(count - 1))
      }
  yield latch

val runPrerequisite: CountdownLatch => IO[Unit] = cl =>
  IO.sleep(100.millis) >> cl.decrement.d.void

val p11 = for
  cl <- CountdownLatch.make(5)
  _ <- IO.race(runPrerequisite(cl).foreverM, cl.await.d)
yield ()

p11.unsafeRunSync()
