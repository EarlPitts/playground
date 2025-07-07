import cats.effect.*
import cats.*
import cats.implicits.*
import cats.Id
import cats.effect.std.Console

import cats.effect.unsafe.implicits.global

// This is basically a polymorphic record of functions
case class GreetingsHandle[F[_]](
    sayHiPurely: String => String,
    sayHiImpurely: String => F[Unit]
)
val realHandle = GreetingsHandle(
  name => s"Hello $name!",
  name => IO.println(s"Hello $name!")
)

val mockHandle = GreetingsHandle(
  _ => s"Hello Geza!",
  _ => IO.unit
)

realHandle.sayHiPurely("jani")
realHandle.sayHiImpurely("jani")
mockHandle.sayHiPurely("jani")
mockHandle.sayHiImpurely("jani")

// In you programs, you can use the handler that was passed to access the service
def helloUser[F[_]: Monad: Console]: GreetingsHandle[F] => F[Unit] = handle =>
  for
    _ <- Console[F].println("What's your name?")
    name <- "jani".pure[F]
    _ <- handle.sayHiImpurely(name)
  yield ()

helloUser(realHandle).unsafeRunSync()
helloUser[IO](Greeter.realGreeter).unsafeRunSync()

// You can also get basically the same thing with traits
trait Greeter[F[_]]:
  val sayHiPurely: String => String
  val sayHiImpurely: String => F[Unit]

object Greeter:
  def realGreeter[F[_]: Monad: Console] = new Greeter[F]:
    val sayHiPurely = name => s"Hello $name!"
    val sayHiImpurely = name => Console[F].println(s"Hello $name!")

  def mockGreeter[F[_]: Monad] = new Greeter[F]:
    val sayHiPurely = _ => s"Hello Geza!"
    val sayHiImpurely = _ => ().pure

Greeter.realGreeter.sayHiPurely("jani")
