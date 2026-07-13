import cats.*
import cats.implicits.*

import cats.effect.*
import cats.effect.implicits.*
import cats.effect.unsafe.implicits.global

import MyError.*
import Result.*

enum MyError extends Throwable:
  case InvalidId
  case NoTemplateId

enum Result:
  case Failure
  case Success

case class Segment(id: Int, templateId: Option[Int])

def loadSegment(id: Int): IO[Segment] =
  val segment =
    if id > 10 then Segment(id, None)
    else Segment(id, Some(10))
  IO.pure(segment)

def log(msg: String): IO[Unit] = IO(println(msg))

def loadTemplate(id: Int): IO[String] = IO.pure("template")

def resolveTemplate(template: String): IO[String] =
  IO.pure("query")

def runQuery(query: String): IO[Int] = IO.pure(100)

def validateSegment(
    segment: Segment
): Either[MyError, Segment] =
  if segment.id < 0
  then InvalidId.asLeft
  else if segment.templateId.isEmpty
  then NoTemplateId.asLeft
  else segment.asRight

def runSegment(
    segment: Segment
): IO[Unit] = IO.unit

def run: IO[Result] = for
  segment <- loadSegment(3)
  result <- validateSegment(segment) match
    case Left(NoTemplateId) =>
      log("No template id found!").as(Failure)
    case Left(InvalidId) =>
      log("Invalid id!").as(Failure)
    case Right(_) => runSegment(segment).as(Success)
yield result

run.unsafeRunSync()
