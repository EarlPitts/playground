package MiscEffects

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.std.Console

import concurrent.duration._

object App extends IOApp.Simple {

  def run: IO[Unit] = for {
    _ <- loop.timeoutTo(3.seconds, IO.println("Nooo!"))
    _ <- IO.interruptible(1 + 1)
    res <- asyncStuff

    // _ <- res match
    //   case Left(value) => IO.println(s"OH FUCK! A $value")
    //   case Right(_)    => IO.println("Success!")
  } yield res

  import java.util.concurrent.{Executors, TimeUnit}

  val scheduler = Executors.newScheduledThreadPool(1)

  val asyncStuff = IO.async_ { cb =>
    scheduler.schedule(new Runnable {
      def run = cb(Right(()))
    }, 500, TimeUnit.MILLISECONDS)
    ()
  }

  lazy val loop: IO[Unit] = IO.println("a") >> IO.sleep(1.second) >> loop
}

// class StdConsole[F[_]: Sync] extends Console[F] {
//   def println[A](a: A)(implicit S: Show[A]): F[Unit] = ???
// }
  
object App2 extends IOApp.Simple {

  def program[F[_]: Monad: Console]: F[Unit] =
    for {
      _    <- Console[F].println("Enter your name:")
      name <- Console[F].readLine
      _    <- Console[F].println(s"Hello $name")
    } yield ()

  def run: IO[Unit] =
    program
}

@main
def main: Unit =  {
  import concurrent.{ Await, Future }
  import concurrent.duration._
  import concurrent.ExecutionContext.Implicits.global

  // val fin = for {
  //   res1 <- Future { Thread.sleep(2000); 2 }
  //   res2 <- Future { Thread.sleep(2000); 2 }
  //   res3 <- Future { Thread.sleep(2000); 2 }
  //   res4 <- Future { Thread.sleep(2000); 2 }
  // } yield res1 + res2 + res3 + res4

  val f1 = Future { Thread.sleep(3000); 2 }
  val f2 = Future { Thread.sleep(1000); 2 }
  val f3 = Future { Thread.sleep(3000); 2 }
  val f4 = Future { Thread.sleep(1000); 2 }

  val fin = for {
    res1 <- f1
    res2 <- f2
    res3 <- f3
    res4 <- f4
  } yield res1 + res2 + res3 + res4

  println(Await.result(fin, 10.second))
}
