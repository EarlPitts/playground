package effects

import cats.effect.{IO, IOApp, ExitCode, Resource, Sync}
import cats.effect.unsafe.implicits._
import cats.effect.implicits._
import scala.concurrent.duration._
import scala.concurrent._
import scala.io.Source
import cats.syntax.all._
import cats.effect.kernel.Deferred
import java.util.concurrent.Executors
import scala.util.Success
import scala.util.Failure
import java.util.concurrent.CompletableFuture
import java.io.RandomAccessFile

object Assert extends App:
  assert(IO.raiseError(new RuntimeException("oh noes")).attempt.unsafeRunSync().isLeft) // Success
  IO(2).map(i => assert(i > 3)).unsafeRunSync()
  assert(IO(-2).unsafeRunSync() > 0)

object Email extends IOApp.Simple:
  trait EmailDelivery:
    type EmailAddress = String
    type Email = String
    def send(to: EmailAddress, email: Email): IO[String]

  implicit val FailingEmailDelivery: EmailDelivery = new EmailDelivery:
    def send(to: EmailAddress, email: Email): IO[String] =
      IO.raiseError(new RuntimeException(s"couldn't send email to $to"))

  implicit val SuccessfulEmailDelivery: EmailDelivery = new EmailDelivery:
    def send(to: EmailAddress, email: Email): IO[String] =
      IO(s"email sent to $to")

  def UserRegistration(implicit emailDelivery: EmailDelivery) =
    emailDelivery.send("something@example.com", "hey!")

  def run: IO[Unit] = for
    _ <- UserRegistration(using SuccessfulEmailDelivery).flatMap(IO.println(_))
    _ <- UserRegistration(using FailingEmailDelivery).flatMap(IO.println(_))
  yield ()
