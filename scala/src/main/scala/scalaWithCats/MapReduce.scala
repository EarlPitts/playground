package MapReduce

import cats._
import cats.implicits._

object MapReduce {
  import concurrent.Future
  import concurrent.Await
  import concurrent.duration._
  import concurrent.ExecutionContext.Implicits.global

  def foldMap[F[_]: Foldable: Functor, A, B: Monoid](v: F[A])(f: A => B) =
    v.map(f).foldLeft(Monoid[B].empty)(Monoid[B].combine)

  println(foldMap(Vector(1, 2, 3, 4))(x => x + 1))
  println(Runtime.getRuntime.availableProcessors)
  println((1 to 15).toList.grouped(5).toList)

  def parallelFoldMap[F[_]: Foldable: Functor, A, B: Monoid](v: F[A])(
      f: A => B
  ): Future[B] =
    val cpuNum = Runtime.getRuntime.availableProcessors
    val batches = v.toList.grouped(cpuNum).toList
    batches
      .traverse(batch => Future(batch.foldLeft(Monoid[B].empty)(_ |+| f(_))))
      .map(_.combineAll)

  val start = System.currentTimeMillis
  println(foldMap((1 to 100000).toList)(x => x + 1))
  println(System.currentTimeMillis - start)
  println(
    Await.result(parallelFoldMap((1 to 100000).toList)(x => x + 1), 1.second)
  )
  println(System.currentTimeMillis - start)
}

@main
def main: Unit =
  MapReduce
