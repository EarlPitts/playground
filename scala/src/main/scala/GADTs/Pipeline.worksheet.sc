enum Pipeline[A, B]:
  case Step[A,B,C](f: A => B, next: Pipeline[B, C]) extends Pipeline[A,C]
  case Empty[A]() extends Pipeline[A, A]
import Pipeline.*

def @>[A,B,C]: (A => B) => Pipeline[B,C] => Pipeline[A,C] = f => pipeline => Step(f, pipeline)
def empty[A] = Empty[A]()

def exec[A,B](pipeline: Pipeline[A,B], a: A): B = pipeline match
  case Empty() => a
  case Step(f, next) => exec(next,(f(a)))


val p = @>[Int, Int, Int]((n: Int) => n + 1)(empty)

exec(p, 5)
