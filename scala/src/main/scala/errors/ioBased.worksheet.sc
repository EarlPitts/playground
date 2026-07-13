import cats.*
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

def runSegment(segment: Segment): IO[Unit] =
  if segment.id < 0
  then InvalidId.raiseError
  else if segment.template.isEmpty
  then NoTemplate.raiseError
  else IO.unit

def run: IO[Result] = for
  segment <- loadSegment(12)
  result <- runSegment(segment)
    .as(Success)
    .handleErrorWith {
      case NoTemplate =>
        log("No template found!").as(Failure)
      case InvalidId =>
        log("Invalid id!").as(Failure)
    }
yield result

run.unsafeRunSync()
