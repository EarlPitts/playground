package Logging

import cats.*
import cats.implicits.*
import cats.effect.*

import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.*

implicit val logger: SelfAwareStructuredLogger[IO] = Slf4jLogger.getLogger[IO]

// Arbitrary Local Function Declaration
def doSomething: IO[Unit] =
  Logger[IO].info("Logging Start Something") *>
  Logger[IO].debug("Logging Start Something") *>
  Logger[IO].warn("Logging Start Something") *>
  Logger[IO].error("Logging Start Something") *>
  Sync[IO].delay(println("I could be doing anything"))
  Async[IO].delay(println("I could be doing anything"))
    .attempt.flatMap{
      case Left(e) => Logger[IO].error(e)("Something Went Wrong")
      case Right(_) => Sync[IO].pure(())
    }

def safelyDoThings[F[_]: Sync]: F[Unit] = for {
  logger <- Slf4jLogger.create[F]
  _ <- logger.info("Logging at start of safelyDoThings")
  something <- Sync[F].delay(println("I could do anything"))
    .onError{case e => logger.error(e)("Something Went Wrong in safelyDoThings")}
  _ <- logger.info("Logging at end of safelyDoThings")
} yield something

def passForEasierUse[F[_]: Sync: Logger] = for {
  _ <- Logger[F].info("Logging at start of passForEasierUse")
  something <- Sync[F].delay(println("I could do anything"))
    .onError{case e => Logger[F].error(e)("Something Went Wrong in passForEasierUse")}
  _ <- Logger[F].info("Logging at end of passForEasierUse")
} yield something

object App extends IOApp.Simple:
  def run: IO[Unit] = doSomething

