import cats.*
import cats.data.*
import cats.implicits.*
import cats.effect.*
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util._

import cats.effect.kernel.Par
import cats.effect.kernel.instances.AllInstances
import cats.effect.testkit.*

import cats.effect.unsafe.implicits.global
import cats.effect.kernel.Outcome.*
import java.util.concurrent.Executor
import java.util.concurrent.Executors
import cats.effect.unsafe.IORuntime
import java.util.concurrent.CompletableFuture
import scala.io.Source
import scala.io.BufferedSource
implicit val ec: scala.concurrent.ExecutionContext =
  scala.concurrent.ExecutionContext.global

case class MyIO[A](unsafeRun: () => A):
  def flatMap[B](f: A => MyIO[B]): MyIO[B] =
    MyIO(() => f(unsafeRun()).unsafeRun())

  def map[B](f: A => B): MyIO[B] =
    flatMap(a => MyIO(() => f(a)))

object MyIO:
  def delay[A](thunk: => A): MyIO[A] = new MyIO(() => thunk)
  def pure[A](value: A): MyIO[A] = new MyIO(() => value)
  def putStr(s: => String): MyIO[Unit] =
    MyIO(() => println(s))

val clock: IO[Long] = IO(System.currentTimeMillis())

def time[A](action: IO[A]): IO[A] = for
  before <- clock
  res <- action
  after <- clock
  duration = after - before
  _ <- IO(println(s"took $duration"))
yield res

def timeFut[A](action: => A): A =
  val before = System.currentTimeMillis()
  val res = action
  println(System.currentTimeMillis() - before)
  res

def fut1 = Future {
  Thread.sleep(100); println(Thread.currentThread().getName())
}
def fut2 = Future {
  Thread.sleep(100); println(Thread.currentThread().getName())
}

def x = fut1.flatMap(_ => fut2)

// timeFut(Await.result(x, 1.second))
// timeFut(Await.result(x, 1.second))

// time(IO.fromFuture(IO(x))).unsafeRunSync()
// time(IO.fromFuture(IO(x))).unsafeRunSync()

// def fut = (fut1, fut2).mapN((_, _) => ())

// Await.result(fut, 1.second)

val io1 = IO { Thread.sleep(100); println(Thread.currentThread.getName) }
val io2 = IO { Thread.sleep(100); println(Thread.currentThread.getName) }

time(io1.flatMap(_ => io2)).unsafeRunSync()

time((io1, io2).parMapN((_, _) => ())).unsafeRunSync()

val p = Par.ParallelF(IO(println("a")))
Par.ParallelF.value(p).unsafeRunSync()

Parallel[IO]

val prog = IO({ println("ize") })
  .flatMap(_ => IO({ println("bigyo"); IO.pure("bigyo") }))
  .d

val par =
  List.fill(2)(()).parTraverse_(_ => prog).d
  // List.fill(2)(prog)
  //   .parSequence_
  //   .d
  // prog.parReplicateA_(2).d

par.unsafeRunSync()

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

IO(2).d.unsafeRunSync()

List
  .range(0, 1000)
  .parTraverse(n => IO.blocking(Thread.sleep(1)) >> IO.pure(n))
  .t
  .void
  .unsafeRunSync()

val ok = IO.sleep(5.micro) >> IO("hi").d
val ko1 = IO.raiseError(new Exception("oh!")).d
val ko2 = IO.raiseError(new Exception("noes!")).d

val e1 = (ok, ko1).parMapN((_, _) => ())
val e2 = (ko1, ok).parMapN((_, _) => ())
val e3 = (ko1, ko2).parMapN((_, _) => ())

e2.attempt.d.unsafeRunSync()

(ok, ko1.attempt.d, ko2.attempt.d).tupled.void.unsafeRunSync()

def bigWork[A] = (a: A) => IO.sleep(100.milli) >> IO.pure(a)

NonEmptyList.of(1, 2, 3).reduce

NonEmptyList
  .of(1, 2, 3, 4, 5)
  .parReduceMapA(n => bigWork(n).d)
  .t
// .unsafeRunSync()

val failing: IO[Unit] =
  IO.sleep(2.second).d >> (new Exception).raiseError

def timeout[A](ioa: IO[A], timeout: FiniteDuration): IO[A] = for
  done <- IO.race(ioa.d, IO.sleep(timeout).d)
  res <- done match
    case Left(value)  => value.pure[IO]
    case Right(value) => new Exception("timed out").raiseError[IO, A]
yield res

def task(n: Int): IO[Unit] = IO.sleep(500.millis).d >> IO.cede.d >> IO(n).d.void

global

val ticker: IO[Unit] = for
  _ <- IO("tick").d
  _ <- IO.blocking(Thread.sleep(500))
  _ <- IO("tock").d
  _ <- IO.sleep(500.millis)
  _ <- ticker
yield ()

// val run = for
//   f <- ticker.start
//   _ <- IO.cede
//   _ <- IO(Thread.sleep(2500))
//   _ <- f.cancel
// yield ()

// run.unsafeRunSync()

val ec2 = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(3))

val r = IO.async_ { cb =>
  Future(println(Thread.currentThread.getName)).onComplete { result =>
    cb(result.toEither)
  }
}

def run = for
  fib <- r.d.start
  _ <- IO("ize").d
  outcome <- fib.join
yield outcome

val run2 = IO.fromFuture {
  IO(Future(println(Thread.currentThread.getName)))
}

IO(Thread.sleep(1000))
  .guarantee(IO("done").d.void)
  .start
  .flatMap(fib => fib.cancel)
  .unsafeRunSync()

run.unsafeRunSync()

val async = IO.async_ { cb =>
  Future(println(Thread.currentThread.getName)).onComplete { res =>
    println(res)
    println(Thread.currentThread.getName)
    cb(Right(1))
  }
}

val ass = for
  fib <- async.d.start
  _ <- IO(println(Thread.currentThread.getName)).d
  a <- fib.join
yield a

ass.unsafeRunSync()

val effect: IO[String] =
  fromCF(IO(cf()))

import scala.jdk.FunctionConverters.*

def fromCF[A](cfa: IO[CompletableFuture[A]]): IO[A] =
  cfa.flatMap { fa =>
    IO.async_ { cb =>
      val handler: (A, Throwable) => Unit = (a: A, error: Throwable) =>
        if a == null then cb(Left(error)) else cb(Right(a))
      fa.handle(handler.asJavaBiFunction)
    }
  }

def cf(): CompletableFuture[String] =
  CompletableFuture.supplyAsync(() => "woo!")

val stringResource =
  Resource
    .make(IO(println("acquiring resource")).as("stringResource"))(_ =>
      IO(println("releasing resource"))
    )

def fileResource(fileName: String): Resource[IO, BufferedSource] =
  Resource
    .make {
      IO.blocking { println("opening file"); Source.fromFile(fileName) }
    } { file =>
      IO.blocking { println("closing file"); file.close }
    }

val p1 = fileResource("build.sbt").use { res =>
  IO(println(s"using the resource: $res")) >>
    IO.blocking(res.getLines.toList) >>= (lines =>
    IO(println(s"file has ${lines.size} lines"))
  )
}

val resources = for
  f1 <- fileResource("sajt.txt")
  f2 <- fileResource("sajt.txt")
yield (f1, f2)

val f1 = fileResource("sajt.txt")
val f2 = fileResource("sajt.txt")

val resources2 = (f1, f2).parTupled

val p2 = resources2.use { case (f1, f2) =>
  IO(println(s"${f1} has ${f1.getLines.size} lines")) *>
    // (new Exception).raiseError >>
    IO(println(s"${f2} has ${f2.getLines.size} lines"))
}

// p2.attempt.unsafeRunSync()

import java.io.RandomAccessFile

class FileBufferReader private (in: RandomAccessFile):
  def readBuffer(offset: Long): IO[(Array[Byte], Int)] =
    IO {
      in.seek(offset)

      val buf = new Array[Byte](FileBufferReader.bufferSize)
      val len = in.read(buf)

      (buf, len)
    }

  private def close: IO[Unit] = IO(in.close())

object FileBufferReader:
  val bufferSize = 4096

  def makeResource(fileName: String): Resource[IO, FileBufferReader] =
    Resource.make {
      IO(new FileBufferReader(new RandomAccessFile(fileName, "r")))
    } { res =>
      res.close
    }

val fbr = FileBufferReader.makeResource("build.sbt")

val p4 =
  (fbr, f1).parTupled
    .evalMap { resources =>
      IO("valami ize").d.as(resources)
    }
    .use { case (x, y) =>
      for
        _ <- x.readBuffer(10).map(_.bimap(_.take(10).toList, identity)).d
        _ <- IO(y.getLines.take(10).toList).d
      yield ()
    }

p4.unsafeRunSync()

val res = Resource.eval(IO(2))
res
  .use { n =>
    IO(n)
  }
  .unsafeRunSync()

Resource.liftK(IO(2)).use { n =>
  IO(n)
}

val backgroundTask: Resource[IO, Unit] =
  val loop = (IO("looping...").d *> IO.sleep(90.millis)).foreverM

  // loop.background.void // This is equivalent

  Resource
    .make(IO("> forking backgroundTask").d *> loop.start)(
      IO("< canceling backgroundTask").d *> _.cancel
    )
    .void

val p5 = for
  _ <- IO("main fiber").d
  _ <- backgroundTask.use { _ =>
    IO("done something while the bakground task is running").d *>
      IO.sleep(200.millis) *>
      IO("other task is done").d
  }
  _ <- IO("all done").d
yield ()

// p5.unsafeRunSync()

case class Config private (connectURL: String)

object Config:
  def fromSource(source: Source): IO[Config] =
    for
      config <- IO(Config(source.getLines.next))
      _ <- IO(s"read $config").d
    yield config

trait DbConnection:
  def query(sql: String): IO[String]

object DbConnection:
  def make(connectURL: String): Resource[IO, DbConnection] =
    Resource.make(
      IO(s"> opening Connection to $connectURL").d *> IO(
        new DbConnection {
          def query(sql: String): IO[String] =
            IO(s"""(results for SQL "$sql")""")
        }
      )
    )(_ => IO(s"< closing Connection to $connectURL").d.void)

lazy val sourceResource: Resource[IO, Source] =
  val config = "exampleConnectURL"

  Resource.make(
    IO("> opening Source to config").d *> IO(Source.fromString(config))
  )(source => IO("< closing Source to config").d *> IO(source.close))

lazy val configResource: Resource[IO, Config] =
  Resource.liftK(sourceResource.use(Config.fromSource(_)))

// lazy val configResource: Resource[IO, Config] = for
//   source <- sourceResource
//   config <- Resource.liftK(Config.fromSource(source))
// yield config

val dbConnectionResource: Resource[IO, DbConnection] = for
  config <- configResource
  conn <- DbConnection.make(config.connectURL)
yield conn

val p6 = dbConnectionResource.use { conn =>
  conn.query("SELECT * FROM users WHERE id = 12").d
}

// p6.unsafeRunSync()

trait DepA:
  def doSomething: IO[Int]

object DepA:
  val acquire = IO("acquire depA").d >> IO(new DepA { def doSomething = IO(2) })
  def finalize(res: DepA): IO[Unit] = IO("release depA").d.void

  def make: Resource[IO, DepA] =
    Resource.make(acquire)(finalize(_))

trait DepB:
  def doSomethingElse: IO[Int]

object DepB:
  val acquire = IO("acquire depB").d >> IO(new DepB {
    def doSomethingElse = IO(3)
  })
  def finalize(res: DepB): IO[Unit] = IO("release depB").d.void

  def make: Resource[IO, DepB] =
    Resource.make(acquire)(finalize(_))

val resourceA: Resource[IO, DepA] = DepA.make
val resourceB: Resource[IO, DepB] = DepB.make

val resourcesAB: Resource[IO, (DepA, DepB)] =
  (resourceA, resourceB).tupled

def applicationLogic(a: DepA, b: DepB): IO[Int] = for
  n <- a.doSomething
  m <- b.doSomethingElse
yield n + m

// val p7 = resourcesAB.use { case (a, b) =>
//   applicationLogic(a, b)
// }.unsafeRunSync()

def err = IO(throw new Exception)

val p8 = for
  t1 <- IO.realTime
  _ <- IO.sleep(200.millis)
  t2 <- IO.realTime
yield (t1, t2)

TestControl
  .execute(p8)
  .flatMap { control =>
    for
      _ <- control.advance(100.millis)
      _ <- control.tick
      _ <- control.advance(100.millis)
      _ <- control.tick
      _ <- control.advance(100.millis)
      _ <- control.tick
      res <- control.results
    yield res
  }
// .unsafeRunSync()

var number = 0

def incr(r: Ref[IO, Int]): IO[Unit] = {
  IO(number += 1) *>
    r.update(_ + 1)
}

val p9 = for
  d <- Deferred[IO, Int]
  _ <- (IO.sleep(100.millis) *> d.complete(3)).start
  res <- d.get.d
yield res

// p9.t.unsafeRunSync()

def ticking(r: Ref[IO, Int], d: Deferred[IO, Unit]): IO[Unit] = for
  _ <- IO.sleep(10.millis)
  _ <- IO(System.currentTimeMillis).d
  count <- r.updateAndGet(_ + 1)
  _ <- IO(s"Current count: $count").d
  _ <- IO.whenA(count >= 13)(d.complete(()).void)
  _ <- ticking(r, d)
yield ()

def beep(d: Deferred[IO, Unit]): IO[Unit] = for
  _ <- d.get
  _ <- IO("beep!").d
yield ()

val p10 = for
  r <- Ref[IO].of(0)
  d <- Deferred[IO, Unit]
  _ <- IO.race(ticking(r, d), beep(d))
yield ()

// p10.unsafeRunSync()
