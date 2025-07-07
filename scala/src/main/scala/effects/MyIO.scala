package effects

case class MyIO[A](runIO: () => A):
  def map[B](f: A => B): MyIO[B] =
    MyIO(() => f(runIO()))

  def flatMap[B](f: A => MyIO[B]): MyIO[B] =
    MyIO(() => f(runIO()).runIO())

object MyIO:
  def putStr(s: => String): MyIO[Unit] =
    MyIO(() => println(s))

  // def readLine(s: => String): MyIO[Unit] =
  //   MyIO(() => println(s))

object Printing extends App:
  val hello = MyIO.putStr("hello!")
  val world = MyIO.putStr("world!")

  val helloWorld = hello.flatMap(_ => world)

  val helloWorld2: MyIO[Unit] = for
    _ <- hello
    _ <- world
  yield ()

  // hello.map((s: String) => s.toUpperCase())

  helloWorld2.runIO()

object Timing extends App:
  import scala.concurrent.duration.FiniteDuration
  import scala.concurrent.duration.MILLISECONDS

  val clock: MyIO[Long] =
    MyIO(() => System.currentTimeMillis())

  def time[A](action: MyIO[A]): MyIO[(FiniteDuration, A)] = for
    startTime <- clock
    a <- action
    endTime <- clock
  yield (FiniteDuration(endTime - startTime, MILLISECONDS), a)

  val timedHello = Timing.time(MyIO.putStr("hello"))

  timedHello.runIO() match
    case (duration, _) => println(s"'hello' took $duration")
