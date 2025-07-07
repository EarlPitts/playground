package felines.CatsEffects

import cats.effect.{IO, IOApp, ExitCode, Resource, Sync}
import cats.effect.unsafe.implicits._
import scala.concurrent.duration._
import scala.io.Source
import cats.syntax.all._

import java.io._

object Main extends IOApp {

  def inputStream[F[_]: Sync](f: File): Resource[F, FileInputStream] =
    val acquire = Sync[F].blocking(new FileInputStream(f))
    val release =
      (inStream: FileInputStream) =>
        Sync[F].blocking(inStream.close()) // .handleErrorWith(_ => IO.unit)
    Resource.make(acquire)(release)

  // This is easier for types that implement AutoCloseable, but
  // you cannot control what happens with exceptions
  def inputStreamAuto(f: File): Resource[IO, FileInputStream] =
    val acquire = IO(new FileInputStream(f))
    Resource.fromAutoCloseable(acquire)

  def outputStream[F[_]: Sync](f: File): Resource[F, FileOutputStream] =
    val acquire = Sync[F].blocking(new FileOutputStream(f))
    val release =
      (outStream: FileOutputStream) =>
        Sync[F].blocking(outStream.close()) // .attempt.void
    Resource.make(acquire)(release)

  def inputOutputStreams[F[_]: Sync](
      in: File,
      out: File
  ): Resource[F, (InputStream, OutputStream)] =
    for {
      in <- inputStream(in)
      out <- outputStream(out)
    } yield (in, out)

  def transfer[F[_]: Sync](
      origin: InputStream,
      destination: OutputStream,
      bufferSize: Int
  ): F[Long] =
    def go(buffer: Array[Byte], acc: Long): F[Long] =
      for {
        amount <- Sync[F].blocking(origin.read(buffer, 0, buffer.size))
        count <-
          if amount > -1
          then
            Sync[F].blocking(destination.write(buffer, 0, amount)) >> go(
              buffer,
              acc + amount
            )
          else Sync[F].pure(acc)
      } yield count
    go(new Array[Byte](bufferSize), 0L)

  def copy[F[_]: Sync](
      origin: File,
      destination: File,
      bufferSize: Int
  ): F[Long] =
    inputOutputStreams(origin, destination).use { case (in, out) =>
      transfer(in, out, bufferSize)
    }

  def copyBracket(origin: File, destination: File, bufferSize: Int): IO[Long] =
    (IO(FileInputStream(origin)), IO(FileOutputStream(destination))).tupled
      .bracket { case (in, out) =>
        transfer(in, out, bufferSize)
      } { case (in, out) =>
        (IO(in.close()), IO(out.close())).tupled.void.handleErrorWith(_ =>
          IO.unit
        )
      }

  def checkArgs(args: List[String]): IO[Unit] =
    if args.length < 2 then
      IO.raiseError(
        IllegalArgumentException("Need origin and destination files")
      )
    else if args(0) == args(1) then
      IO.raiseError(
        IllegalArgumentException("Files have to differ")
      )
    else IO.unit

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- checkArgs(args)
      bufferSize = 1024 * 10
      orig = File(args(0))
      dest = File(args(1))
      count <- copy[IO](orig, dest, bufferSize)
      _ <- IO.println(
        s"$count bytes copied from ${orig.getPath} to ${dest.getPath}"
      )
    } yield ExitCode.Success
}

object Test extends IOApp {
  def run(args: List[String]): IO[ExitCode] = helloWorld.map(_ => ExitCode.Success) // >> IO.pure(ExitCode.Success)

  def helloWorld: IO[Unit] = IO.println("hello") >> IO.println("world")
}
