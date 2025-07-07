package Fs2

import fs2.Stream
import cats.effect.*
import scala.concurrent.duration.*

object App extends IOApp.Simple:

  val p = IO.println("hello")

  val a = Stream.repeatEval(p)
        .take(10)
        .metered(1.second)

  def run: IO[Unit] = for
    _ <- IO.println("##########################")
    _ <- a.compile.toList
    _ <- IO.println("##########################")
  yield ()
