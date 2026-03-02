package home.textboard

import cats.effect.{IO, IOApp}

object Main extends IOApp.Simple:
  val run = TextboardServer.run[IO]
