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

val stringResource: Resource[IO, String] =
  import debug.*

  Resource.make(
    IO("> acquiring stringResource").myDebug *> IO("String")
  )(_ => IO("< releasing stringResource").myDebug.void)

val intResource: Resource[IO, Int] =
  import debug.*

  Resource.make(
    IO("> acquiring intResource").myDebug *> IO(99)
  )(_ => IO("< releasing intResource").myDebug.void)

object Resources extends IOApp.Simple:
  import debug.*

  def run: IO[Unit] =
    // Resource is a functor
    stringResource
      .map(_.toUpperCase)
      .use { s =>
        IO(s"$s is so cool!").myDebug
      }
      .void

object ResourceFailure extends IOApp.Simple:
  import debug.*

  def run: IO[Unit] =
    stringResource
      .use(_ => IO.raiseError(new RuntimeException("oh noes!")))
      .attempt
      .myDebug
      .void

object FileResource extends IOApp.Simple:
  def run: IO[Unit] = for
    _ <- IO.println("Making resource")
    r = FileBufferReader.makeResource("build.sbt")
    data <- r.use(f => f.readBuffer(100))
    _ <- IO.println((data._1.map(_.toChar)).mkString)
  yield ()

  class FileBufferReader private (in: RandomAccessFile):
    def readBuffer(offset: Long): IO[(Array[Byte], Int)] =
      IO {
        in.seek(offset)

        val buf = new Array[Byte](FileBufferReader.bufferSize)
        val len = in.read(buf)

        (buf, len)
      }
    private def close: IO[Unit] = IO(in.close())

  object FileBufferReader:
    val bufferSize = 4096

    def makeResource(fileName: String): Resource[IO, FileBufferReader] =
      Resource.make(
        IO(new FileBufferReader(new RandomAccessFile(fileName, "r")))
      )(res => res.close)

object BackgroundTaskResource extends IOApp.Simple:
  import debug.*

  def run: IO[Unit] = for
    _ <- backgroundTask.use { _ =>
      IO("other work while background task is running").myDebug >>
        IO.sleep(200.millis) >>
        IO("other work done").myDebug
    }
    _ <- IO("all done").myDebug
  yield ()

  val backgroundTask: Resource[IO, Unit] =
    val loop =
      (IO("looping...").myDebug >> IO.sleep(100.millis)).foreverM

    // background is equivalent to make(loop.start)(_.cancel)
    loop.background.void
    // Resource
    //   .make(IO("> forking backgroundTask").myDebug >> loop.start)(
    //     IO("< canceling backgroundTask").myDebug.void >> _.cancel
    //   ).void

object BasicResourceComposed extends IOApp.Simple:
  import debug.*

  def run: IO[Unit] =
    (stringResource, intResource).parTupled.use { case (s, i) =>
      IO(s"$s is so cool!").myDebug >>
        IO(s"$i is also cool!").myDebug
    }.void

object EarlyRelease extends IOApp.Simple:
  import debug.*

  def run: IO[Unit] = for
    config <- configResource.use(IO(_))
    res <- dbConnectionResource(config).use(conn => conn.query("SELECT * FROM users WHERE id = 12").myDebug)
  yield ()

  def dbConnectionResource(config: Config): Resource[IO, DbConnection] =
    DbConnection.make(config.connectURL)

  lazy val configResource: Resource[IO, Config] = for
    source <- sourceResource
    config <- Resource.liftK(Config.fromSource(source))
  yield config

  lazy val sourceResource: Resource[IO, Source] =
    Resource.make(
      IO(s"> opening Source to config")
        .myDebug >> IO(Source.fromString(config))
        )(source => IO(s"< closing Source to config").myDebug >> IO(source.close))
  
  val config = "exampleConnectURL"

case class Config(connectURL: String)

object Config:
  import debug.*

  def fromSource(source: Source): IO[Config] = for
    config <- IO(Config(source.getLines().next()))
    _ <- IO(s"read $config").myDebug
  yield config

trait DbConnection:
  def query(sql: String): IO[String]

object DbConnection:
  import debug.*

  def make(connectURL: String): Resource[IO, DbConnection] =
    Resource.make(
      IO(s"> opening Connection to $connectURL").myDebug >> IO(
        new DbConnection {
          def query(sql: String): IO[String] =
            IO(s"""(results for SQL "$sql")""")
        }
      )
  )(_ => IO(s"< closing Connection to $connectURL").myDebug.void)
