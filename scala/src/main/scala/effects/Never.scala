package effects

import cats._
import cats.effect._
import scala.concurrent.duration._

object NeverApp extends IOApp.Simple {
  val run: IO[Unit] = (IO.println("nice") >> IO.sleep(2.second)).foreverM
}
