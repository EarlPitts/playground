package felines.Files

import cats.effect.{IO, Resource, IOApp}
import cats.effect.unsafe.implicits._
import scala.concurrent.duration._
import scala.io.Source
import cats.syntax.all._

import java.io._

def readFile(fileName: String): IO[String] = {
  // Opening a file handle for reading text
  val acquire = IO(Source.fromFile(fileName))
  acquire.bracket(in => IO(in.getLines().mkString))(in =>IO(in.close()))
}

def readFile2(fileName: String): IO[String] = for {
  s <- IO.pure(Source.fromFile(fileName))
  content = s.getLines().mkString
} yield content

object Main extends IOApp.Simple {
  def run = 
    readFile("test.hs") >>= (a => IO.println(a))

  def run2 = for {
    a <- readFile("test.hs")
    _ <- IO.println(a)
  } yield IO.unit

  def run3 = for {
    a <- readFile("test.hs")
  } yield println(a)

}
