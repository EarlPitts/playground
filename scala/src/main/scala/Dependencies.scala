package dependencies

import cats._
import cats.implicits._
import cats.kernel.Comparison._

// https://fsharpforfunandprofit.com/posts/dependencies/

// Dependency Retention
val compareTwoStrings =
  println("Enter the first value")
  val str1 = scala.io.StdIn.readLine()
  println("Enter the second value")
  val str2 = scala.io.StdIn.readLine()

  if str1 > str2 then println("The first value is bigger")
  else if str1 < str2 then println("The first value is smaller")
  else println("The values are equal")

// Dependency Rejection
// We break thing up into effectful parts and pure parts
// and "sandwich" the pure code between impure ones
def logic(str1: String, str2: String): String =
  if str1 > str2 then "The first value is bigger"
  else if str1 < str2 then "The first value is smaller"
  else "The values are equal"

val compareTwoStrings2 =
  println("Enter the first value")
  val str1 = scala.io.StdIn.readLine()
  println("Enter the second value")
  val str2 = scala.io.StdIn.readLine()

  println(logic(str1, str2))

// Dependency Parameterization
val customComparison: Order[String] = new Order[String] {
  def compare(x: String, y: String): Int =
    if x.startsWith("x") then 1 else -1
}

def logic(comparison: Order[String])(str1: String, str2: String): String =
  comparison.comparison(str1, str2) match
    case GreaterThan => "The first value is bigger"
    case LessThan    => "The first value is smaller"
    case EqualTo     => "The values are equal"

def compareTwoStrings3(comparison: Order[String]) =
  println("Enter the first value")
  val str1 = scala.io.StdIn.readLine()
  println("Enter the second value")
  val str2 = scala.io.StdIn.readLine()

  val comparer = logic(comparison)

  println(comparer(str1, str2))

trait Console:
  def write: String => Unit
  def read: () => String

val realConsole = new Console {
  def read: () => String = scala.io.StdIn.readLine
  def write: String => Unit = println
}

val mockConsole = new Console {
  def read: () => String = () => "always this string"
  def write: String => Unit = _ => ()
}

def program(console: Console) =
  console.write("sajt")
  console.read

// Reader
import cats.data.Reader

def logicReader(str1: String, str2: String): Reader[Order[String], String] =
  Reader { cmp =>
    cmp.comparison(str1, str2) match
      case GreaterThan => "The first value is bigger"
      case LessThan    => "The first value is smaller"
      case EqualTo     => "The values are equal"
  }

def compareTwoStrings4 =
  println("Enter the first value")
  val str1 = scala.io.StdIn.readLine()
  println("Enter the second value")
  val str2 = scala.io.StdIn.readLine()

  println(logicReader(str1, str2).run(customComparison))

def ask[A]: Reader[A, A] = Reader(identity)

def logicReader2(str1: String, str2: String): Reader[Order[String], String] = for
  cmp <- ask[Order[String]]
  result = cmp.comparison(str1, str2) match
      case GreaterThan => "The first value is bigger"
      case LessThan    => "The first value is smaller"
      case EqualTo     => "The values are equal"
yield result

@main
def main =
  compareTwoStrings3
