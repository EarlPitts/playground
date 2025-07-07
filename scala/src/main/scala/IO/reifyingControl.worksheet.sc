enum IO[A]:
  case Return(a: A)
  case Suspend(resume: () => A)
  case FlatMap[A, B](
    sub: IO[A], k: A => IO[B]) extends IO[B]

  def flatMap[B](f: A => IO[B]): IO[B] =
    FlatMap(this, f)
  def map[B](f: A => B): IO[B] =
    flatMap(a => Return(f(a)))
  @annotation.tailrec final def unsafeRun(): A = this match
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match
      case Return(a) => f(a).unsafeRun()
      case Suspend(r) => f(r()).unsafeRun()
      case FlatMap(y, g) =>
        y.flatMap(a => g(a).flatMap(f)).unsafeRun()

object IO:
  def apply[A](a: => A): IO[A] =
    suspend(Return(a))

  def suspend[A](ioa: => IO[A]): IO[A] =
    Suspend(() => ioa).flatMap(identity)

def printLine(s: String): IO[Unit] =
  IO(println(s))

val p = printLine("a").flatMap(_ => printLine("a")).unsafeRun()

val g = List.fill(1000000)(identity).foldLeft(identity)(_ compose _)

val f = (x: Int) => IO.Return(x)

val g2 = List.fill(100000)(f).foldLeft(f)((a, b) =>
    x => IO.suspend(a(x).flatMap(b)))
