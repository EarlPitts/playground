import cats.*
import cats.implicits.*
import cats.effect.*

type Sajt[A] = OptionT[IO, A]

val g: EitherT[Sajt, String, List[Int]] = for
  // a <- EitherT(OptionT(IO(Option.empty[Either[String, List[Int]]])))
  a <- EitherT(OptionT(IO(Option(Right(List(1,2,3))))))
  // b <- EitherT(OptionT(IO(Option("sajt".asLeft[List[Int]]))))
  b <- EitherT(OptionT(IO(Option(List(4,5,6).asRight[String]))))
yield a ++ b
