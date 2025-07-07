enum ComparisonResult:
  case Bigger
  case Smaller
  case Equal

import ComparisonResult.*

def compareTwoStrings: String => String => ComparisonResult = str1 => str2 =>
  if str1 > str2
  then Bigger
  else if str1 < str2 then Smaller
  else Equal

def comparisonMessage: ComparisonResult => String = res => res match
  case Bigger => "First is bigger"
  case Smaller => "Second is bigger"
  case Equal => "They are equal"

def comp(str1: String, str2: String): Unit =
  println(comparisonMessage(compareTwoStrings(str1)(str2)))

comp("a", "b")

val a = 1 + 2
def b = { println("a"); 3 }

b + b

lazy val c = { println("a"); 3 }
c + c

compareTwoStrings("a")("b")

import cats.*
import cats.implicits.*
import cats.data.ReaderT
import cats.data.Reader

// def program(n: Int): Reader[String, Int] = Reader { for
//   yield x
// }

import cats.effect.*

case class Conf(value: Int)

val program: ReaderT[IO,Conf,Int] = ReaderT { conf => for
  _ <- IO.println(s"conf: $conf")
  yield 3
}

import cats.effect.unsafe.implicits.global

program.run(Conf(2)).unsafeRunSync()

val f = Reader[Int, Int](x => x + 1).map(_ + 3)

f.run(2)
