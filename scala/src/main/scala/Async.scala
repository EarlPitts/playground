package AsyncApp

import cats.effect.*
import scala.concurrent.duration.*
import cats.effect.std.Dispatcher
import scala.concurrent.Future

import concurrent.ExecutionContext.Implicits.global
import scala.util.*

def asyncRun(): IO[Unit] = IO.async_ { cb =>
  Thread.sleep(1000)
  println("sajt")
  cb(Right(()))
}

def realAsyncRun(): IO[Unit] = IO.async_ { cb =>
  Future {
    Thread.sleep(1000)
    println("sajt")
  }.onComplete(t => t match {
    case Failure(exception) => cb(Left(exception))
    case Success(value) => cb(Right(value))
  })
}

def syncRun(): IO[Unit] = IO.blocking {
  Thread.sleep(1000)
  println("sajt")
}


object Main extends App:
  def run: IO[Unit] =
      IO.blocking(println("a")).start >>
      // realAsyncRun() >>
      // dispatcher.unsafeRunAndForget(asyncRun())
      IO.println("done")
