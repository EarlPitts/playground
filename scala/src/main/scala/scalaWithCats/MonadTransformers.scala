package MonadTransformers

object Transformers {
  import cats._
  import cats.data._
  import cats.implicits._

  type ListOption[A] = OptionT[List, A]

  type ErrorOr[A] = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  println(
    OptionT(List(Option(1))).map(_ + 2): ListOption[Int]
  )
  println(10.pure[ErrorOrOption])
  println(OptionT[ErrorOr, Int](Right(Some(10))))
  println(10.pure[ErrorOrOption].value)
}

object Example {
  import cats.data.Writer

  type Logged[A] = Writer[List[String], A]

  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None      => Writer(List(s"Failed on $str"), None)
    }

  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    import cats.data.OptionT

    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c

    result.value
  }

  println(addAll("1", "2", "3"))
  println(addAll("1", "c", "3"))
}

object Autobots {
  import scala.concurrent.Future
  import scala.concurrent.Await
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  import cats.data.EitherT
  import cats.implicits._

  // type Response[A] = Future[Either[String, A]]

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(level) => EitherT.right(Future(level))
      case None        => EitherT.left(Future(s"$autobot unreachable"))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = for {
    a1 <- getPowerLevel(ally1)
    a2 <- getPowerLevel(ally2)
  } yield (a1 + a2) > 15

  def tacticalReport(ally1: String, ally2: String): String =
    val stack = canSpecialMove(ally1, ally2).value

    Await.result(stack, 1.second) match {
      case Left(msg) => s"Comms error: $msg"
      case Right(true) => s"$ally1 and $ally2 are ready to roll!"
      case Right(false) => s"$ally1 and $ally2 needs recharge"
    }

  println(Await.result(getPowerLevel("Jizz").value, 1.second))
  println(tacticalReport("Jazz", "Hot Rod"))
}

@main
def main: Unit =
  println("-" * 50)
  // Transformers
  // Example
  Autobots
  println("-" * 50)
