import org.http4s.Uri
import scala.util.Try
import cats.effect.*
import cats.implicits.*
import cats.*
import scala.concurrent.duration._

import cats.effect.unsafe.implicits.global

// You have to wait for the first two to complete before the whole stuff is cancelled
(-2 to 2).toList
  .traverse { i =>
    IO(1 / i) >> IO.sleep(1.second)
  }
// .unsafeRunSync()

// Cancelled immediately
(-2 to 2).toList
  .parTraverse { i =>
    IO(1 / i) >> IO.sleep(1.second)
  }
// .unsafeRunSync()

// By wrapping in a Try, you can forego cancellation
(-2 to 2).toList
  .parTraverse { i =>
    for {
      result <- IO(Try((1 / i)).toOption)
      _ <- IO.sleep(100.millis)
    } yield result
  }
// .unsafeRunSync()

// Or just use .attempt
(-2 to 2).toList
  .parTraverse { i =>
    IO(1 / i).attempt <* IO.sleep(100.millis)
  }
// .unsafeRunSync()

def f[F[_]: Async: Parallel] =
  (-2 to 2).toList.parTraverse { i =>
    Sync[F].delay(1 / i) >> Async[F].sleep(1.second)
  }

// f[IO].unsafeRunSync()
