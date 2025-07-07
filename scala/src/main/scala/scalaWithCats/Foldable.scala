package Foldable

import cats._
import cats.implicits._

object FoldableExamples {

  // Reverses the list
  println(List(1, 2, 3).foldLeft(Nil)((xs: List[Int], x: Int) => x :: xs))
  // Leaves it intact
  println(List(1, 2, 3).foldRight(List.empty[Int])(_ :: _))

  println(List(1, 2, 3).foldRight(List(Nil))((x: Int, xs: List[List[Int]]) => {
    val newState = x :: xs.head
    newState :: xs
  }))

  def map[A, B](la: List[A])(f: A => B): List[B] =
    la.foldRight(List.empty[B])(f(_) :: _)

  def flatMap[A, B](la: List[A])(f: A => List[B]): List[B] =
    def join[B](lb: List[List[B]]): List[B] =
      lb.foldRight(List.empty[B])(_ ::: _)
    join(map(la)(f))

  def filter[A](la: List[A])(p: A => Boolean): List[A] =
    la.foldRight(List.empty[A])((a, as) => if (p(a)) a :: as else as)

  def sum: List[Int] => Int = _.foldRight(0)(_ + _)

  Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _)
  // foldRight uses Eval for stack-safety
  println(
    Foldable[List]
      .foldRight(List(1, 2, 3), Eval.now(0)) { (num, eval) =>
        eval.map(_ + num)
      }
      .value
  )

  // Uses the monoid instance
  println(Foldable[List].fold(List(1, 2, 3))) // Alias to combineAll
  println(Foldable[List].foldMap(List(1, 2, 3))(_ + 2))
  println(Foldable[List].foldMap(List(1, 2, 3))(_.toString))

  // println(
  //   (Foldable[List] compose Foldable[Vector]).fold(
  //     List(Vector(1, 2), Vector(3, 4))
  //   )
  // )
}

object TraverseExamples {
  import concurrent._
  import concurrent.duration._
  import concurrent.ExecutionContext.Implicits.global

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60)

  // With folding
  val allUptimes: Future[List[Int]] =
    hostnames.foldLeft(Future(List.empty[Int])) { (accum, host) =>
      val uptime = getUptime(host)
      for {
        accum <- accum
        uptime <- uptime
      } yield accum :+ uptime
    }

  // With traverse
  val allUptimes2: Future[List[Int]] =
    hostnames.traverse(getUptime)
    // hostnames.map(getUptime).sequence
    // Future.traverse(hostnames)(getUptime)

  println(Await.result(allUptimes, 1.second))
  println(Await.result(allUptimes2, 1.second))
}

object TraverseImpl {
  import concurrent._
  import concurrent.duration._
  import concurrent.ExecutionContext.Implicits.global

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60)

  // Combining using an Applicative
  def newCombine(accum: Future[List[Int]], host: String): Future[List[Int]] =
    (accum, getUptime(host)).mapN(_ :+ _)

  def listTraverse[F[_]: Applicative, A, B](
      la: List[A]
  )(f: A => F[B]): F[List[B]] =
    la.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, f(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, B](l: List[F[B]]): F[List[B]] =
    listTraverse(l)(identity)

  println(listSequence(List(Vector(1, 2), Vector(3, 4))))
  println(listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5,6))))

  def process(inputs: List[Int]) =
    listTraverse(inputs)(n => if(n % 2 == 0) Some(n) else None)

  import cats.data.Validated

  type ErrorsOr[A] = Validated[List[String], A]

  def process2(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if (n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }

  println(process2(List(2, 4, 6)))
  println(process2(List(1, 2, 3)))
}

@main
def main: Unit =
  println("-" * 50)
  // FoldableExamples
  // TraverseExamples
  TraverseImpl
  println("-" * 50)
