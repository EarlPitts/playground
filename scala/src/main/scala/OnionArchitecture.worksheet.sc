import cats._
import cats.implicits._
import cats.effect._

import cats.effect.unsafe.implicits.global
import scala.io.Source
import scala.io.BufferedSource

// Outer (infrastructure) layer
// Configs, resources
object Main extends IOApp.Simple:
  def run: IO[Unit] = Configs
    .read[IO]
    .flatMap { conf =>
      FileManager.make[IO](conf.foo).use { fileMgr =>
        val ns = NumberService.make[IO](fileMgr)
        val res = NumberAdder(ns).addThemUp
        res.flatMap(res => IO(println(s"$res")))
      }
    }

final case class Configs(
    foo: String,
    bar: Int,
    baz: String
)

object Configs:
  def read[F[_]: Sync]: F[Configs] =
    (
      Sync[F].delay(sys.env("FILE")),
      Sync[F].delay(sys.env("DO_NOT_TRACK").toInt),
      Sync[F].delay(sys.env("HOME"))
    ).mapN(Configs.apply)

abstract class FileManager[F[_]](val file: BufferedSource):
  def getLines: F[List[String]]

object FileManager:
  def make[F[_]: Sync](
      fileName: String
  ): Resource[F, FileManager[F]] =
    Resource.make(
      Sync[F]
        .blocking(Source.fromFile(fileName))
        .map {
          new FileManager[F](_):
            def getLines: F[List[String]] =
              Sync[F].blocking(file.getLines.toList)
        }
    )(fm => Sync[F].blocking(fm.file.close))

// Middle (service, algebra) layer
sealed trait NumberService[F[_]]:
  def getNumbers: F[List[Int]]

object NumberService:
  def make[F[_]: Functor](
      file: FileManager[F]
  ): NumberService[F] =
    new NumberService[F]:
      def getNumbers: F[List[Int]] =
        file.getLines.map(
          _.collect {
            case line if line.toIntOption.isDefined =>
              line.toInt
          }
        )

// Inner (business logic, preferably pure) layer
case class NumberAdder[F[_]: Functor](
    numbers: NumberService[F]
):
  private def addNums(nums: List[Int]): Int =
    nums.foldl(0)(_ + _)

  def addThemUp: F[Int] = numbers.getNumbers.map(addNums)

// Main.run.unsafeRunSync()
