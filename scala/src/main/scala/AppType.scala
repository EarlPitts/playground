package AppType

import cats.effect.*
import cats.*
import cats.implicits.*
import cats.data.*

case class Env(configs: Map[String, String])

type App = ReaderT[IO, Env, Unit]

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    val env = IO(Env(Map("valami" -> "valami")))
    env >>= program.run

  def program: App = ReaderT { env =>
    IO.println(env.configs.get("valami"))
  }
