import Utils.Debug.syntax.*
import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.implicits.*
import cats.effect.unsafe.implicits.global
import cats.implicits.*

import scala.concurrent.duration.*
import java.util.UUID
import cats.effect.std.Supervisor

type Result[A] = Outcome[IO, Throwable, A]

sealed trait Job[A]

object Job:
  type Handle[A] = Fiber[IO, Throwable, A]

  case class Id(value: UUID) // extends AnyVal

  def create[A](task: IO[A]): IO[Scheduled[A]] =
    IO(Id(UUID.randomUUID)).map(Scheduled(_, task))

  case class Scheduled[A](id: Id, task: IO[A]) extends Job[A]:
    def start: IO[Running[A]] = for
      outcome <- Deferred[IO, Result[A]]
      fiber <- task
        .guaranteeCase(oc => outcome.complete(oc).void)
        .start
    yield Running(id, fiber, outcome)

  case class Running[A](
      id: Id,
      handle: Handle[A],
      outcome: Deferred[IO, Result[A]]
  ) extends Job[A]:
    val await: IO[Completed[A]] =
      outcome.get.map(Completed(id, _))

  case class Completed[A](id: Id, outcome: Result[A]) extends Job[A]

case class State[A](
    maxRunning: Int,
    scheduled: Chain[Job.Scheduled[A]] = Chain.empty[Job.Scheduled[A]],
    running: Map[Job.Id, Job.Running[A]] = Map.empty[Job.Id, Job.Running[A]],
    completed: Chain[Job.Completed[A]] = Chain.empty[Job.Completed[A]]
):
  def enqueue(job: Job.Scheduled[A]): State[A] =
    copy(scheduled = scheduled :+ job)

  def dequeue: (State[A], Option[Job.Scheduled[A]]) =
    if running.size >= maxRunning
    then (this, None)
    else
      scheduled.uncons
        .map { case (head, tail) =>
          (copy(scheduled = tail), Some(head))
        }
        .getOrElse((this, None))

  def running(job: Job.Running[A]): State[A] =
    copy(running = running + (job.id -> job))

  def onComplete(job: Job.Completed[A]): State[A] =
    copy(completed = completed :+ job)

trait JobScheduler[A]:
  def schedule(task: IO[A]): IO[Job.Id]

object JobScheduler:
  def make[A](stateRef: Ref[IO, State[A]]): JobScheduler[A] =
    new JobScheduler[A]:
      def schedule(task: IO[A]): IO[Job.Id] = for
        job <- Job.create(task)
        _ <- stateRef.update(_.enqueue(job))
      yield job.id

trait Reactor[A]:
  def whenAwake(
      onStart: Job.Id => IO[Unit],
      onComplete: (Job.Id, Result[A]) => IO[Unit]
  ): IO[Unit]

object Reactor:
  def make[A](stateRef: Ref[IO, State[A]]): Reactor[A] =
    new Reactor:
      def whenAwake(
          onStart: Job.Id => IO[Unit],
          onComplete: (Job.Id, Result[A]) => IO[Unit]
      ): IO[Unit] =
        def registerOnComplete(job: Job.Running[A]) =
          job.await
            .flatMap(jobCompleted)
            .start

        def startNextJob: IO[Option[Job.Running[A]]] =
          stateRef
            .modify(_.dequeue)
            .flatMap(_.traverse(startJob))

        def jobCompleted(job: Job.Completed[A]): IO[Unit] =
          stateRef
            .update(_.onComplete(job))
            .flatTap(_ => onComplete(job.id, job.outcome).attempt)

        def startJob(scheduled: Job.Scheduled[A]): IO[Job.Running[A]] = for
          running <- scheduled.start
          _ <- stateRef.update(_.running(running))
          _ <- registerOnComplete(running)
          _ <- onStart(running.id).attempt
        yield running

        startNextJob
          .iterateUntil(_.isEmpty)
          .void

trait Zzz:
  def sleep: IO[Unit]
  def wakeUp: IO[Unit]

object Zzz:
  enum State:
    case Awake
    case Asleep
  import State.*

  def make: IO[Zzz] =
    Ref[IO].of(Awake).map { state =>
      new Zzz:
        def sleep: IO[Unit] =
          state.update {
            case Awake  => Asleep
            case Asleep => Asleep
          }
        def wakeUp: IO[Unit] =
          state.update {
            case Asleep => Awake
            case Awake  => Awake
          }
    }

val work = ((IO.sleep(1.second) >> IO(println("juhu!")) >> IO.pure(12))).d

val failingWork =
  (IO.sleep(1.second) >> IO(println("juhu!")) >> (new Exception).raiseError).d

val job = Job.create(work)

val run = for
  scheduled <- job.d
  running <- scheduled.start.d
  done <- running.await.d
yield ()

val ize = IO(Thread.sleep(1000)).guarantee(IO(println("runned"))).uncancelable

val runMe = for
  fib <- ize.start
  _ <- fib.cancel
yield ()

runMe.t.unsafeRunSync()

// val run2 = for
//   scheduler <- JobScheduler.make(10)
//   addJob = scheduler.schedule(IO("whee").d)
//   jobIds <- addJob.replicateA(10)
// yield jobIds
//
// run2.t.unsafeRunSync()

// run.unsafeRunSync()
