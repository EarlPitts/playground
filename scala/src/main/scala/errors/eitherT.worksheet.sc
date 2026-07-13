import cats.*
import cats.data.EitherT
import cats.implicits.*

import cats.effect.*
import cats.effect.implicits.*
import cats.effect.unsafe.implicits.global

import MyError.*
import Result.*

enum MyError extends Throwable:
  case InvalidId
  case NoTemplate

enum Result:
  case Failure
  case Success

case class Segment(id: Int, template: Option[String])

def loadSegment(id: Int): IO[Segment] =
  val segment =
    if id > 10 then Segment(id, None)
    else Segment(id, Some("template"))
  IO.pure(segment)

def log(msg: String): IO[Unit] = IO(println(msg))

def runSegment(
    segment: Segment
): EitherT[IO, MyError, Unit] =
  if segment.id < 0
  then EitherT.leftT(InvalidId)
  else if segment.template.isEmpty
  then EitherT.leftT(NoTemplate)
  else EitherT.pure(())

def run: IO[Result] = for
  segment <- loadSegment(3)
  result <- runSegment(segment).value
    .flatMap {
      case Left(NoTemplate) =>
        log("No template found!").as(Failure)
      case Left(InvalidId) =>
        log("Invalid id!").as(Failure)
      case Right(_) => Success.pure[IO]
    }
yield result

run.unsafeRunSync()
