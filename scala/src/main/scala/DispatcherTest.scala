import io.circe.*
import io.circe.parser.*
import io.circe.optics.JsonPath.*
import cats.effect.std.Dispatcher
import cats.effect._
import cats._
import cats.implicits._
import cats.effect.unsafe.implicits.global
import concurrent.duration._

// You can use evalMap and useForever to run the whole app in the context of
// a never-terminating resource
object Disp extends IOApp.Simple:
  def run = Dispatcher
    .parallel[IO]
    .evalMap { d =>
      IO {
        d.unsafeRunAndForget {
          (IO.println("hey") >>
            IO.sleep(200.millis)).foreverM
        }
      } >>
        IO {
          d.unsafeRunAndForget {
            // IO(throw new RuntimeException) >>
            (IO.println("ho") >>
              IO.sleep(300.millis)).foreverM
          }
        }
    }
    .useForever
