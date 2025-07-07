package intro


import cats.implicits._
import cats._

sealed trait Json
final case class JsObject(value: Map[String, Json]) extends Json
final case class JsString(value: String) extends Json
final case class JsNumber(value: Int) extends Json
case object JsNull extends Json

trait Encodable[A] {
  def encode(a: A): Json
}

object Encodable {
  def apply[A](implicit instance: Encodable[A]): Encodable[A] = instance

  implicit val stringEncode: Encodable[String] =
    new Encodable[String] {
      def encode(s: String): Json = JsString(s)
    }

  implicit val intEncode: Encodable[Int] =
    new Encodable[Int] {
      def encode(n: Int): Json = JsNumber(n)
    }
}

// trait Show[A] {
//   def show(a: A): String
// }
//
// object Show {
//   def apply[A](implicit instance: Show[A]): Show[A] = instance
//
//   implicit val jsonShow: Show[Json] = new Show[Json] {
//     def show(a: Json): String = a match
//       case JsNull => "null"
//       case JsNumber(n) => s"$n"
//       case JsString(s) => s"\"$s\""
//       case JsObject(o) =>
//         s"{${o.toList.map((key, value) => s"\"${key}\": ${show(value)}").mkString(",")}}"
//   }
// }

object Json {
  def toJson[A](value: A)(implicit w: Encodable[A]): Json =
    w.encode(value)
}

object JsonSyntax {
  implicit class JsonEncoderOps[A](value: A) {
    def toJson(implicit e: Encodable[A]): Json =
      e.encode(value)
  }
}

val j = JsObject(Map("sajt" -> JsNull, "nemsajt" -> JsNumber(3), "ize" -> JsObject(Map("string" -> JsString("valami")))))

import JsonSyntax._

// ----------------

trait Printable[A]:
  def format(value: A): String
    
object Printable:
  def apply[A](implicit instance: Printable[A]) = instance

  def format[A](value: A)(implicit instance: Printable[A]): String =
    instance.format(value)

  def print[A](value: A)(implicit instance: Printable[A]): Unit =
    println(format(value))

object PrintableInstances:

  implicit val stringInstance: Printable[String] =
    new Printable[String] {
      def format(value: String): String = value
    }

  implicit val intInstance: Printable[Int] =
    new Printable[Int] {
      def format(value: Int): String = s"$value"
    }
  
case class Cat(name: String, age: Int, color: String)//:
  // def show(implicit instance: Printable[Cat]): String =
  //   instance.format(this)

object Cat:
  import PrintableInstances._

  given Printable[Cat] with
    def format(cat: Cat): String =
      val name = Printable.format(cat.name)
      val age = Printable.format(cat.age)
      val color = Printable.format(cat.color)
      s"$name is a $age year-old $color cat."

  given Show[Cat] with
    def show(cat: Cat): String =
      s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat."

  given Eq[Cat] with
    def eqv(a: Cat, b: Cat): Boolean =
      a.name === b.name &&
      a.age === b.age &&
      a.color === b.color

  // With helper from cats
  // implicit val catShow: Show[Cat] =
  //   Show.show(cat =>
  //       s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat.")

  // implicit val catShow: Show[Cat] =
  //   new Show[Cat]:
  //     def show(cat: Cat): String =
  //       s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat."

// Extension methods to extend existing classes
object PrintableSyntax:

  implicit class PrintableOps[A](value: A):
    def format(implicit instance: Printable[A]): String =
      instance.format(value)

    def print(implicit instance: Printable[A]): Unit =
      println(format(instance))
  
trait Equal[A]:
  def eqv(a: A, b: A): Boolean

  def notEqv(a: A, b: A): Boolean = !eqv(a,b)

object Equal:
  def apply[A](implicit instance: Equal[A]) = instance
  
  given Equal[Int] with
    def eqv(a: Int, b: Int) = a == b

object EqualSyntax:
  implicit class EqualOps[A](value: A):
    def ===(a: A)(implicit instance: Equal[A]) = instance.eqv(value, a)
    def =!=(a: A)(implicit instance: Equal[A]) = !(value === a)

def sajt: List[String] =
  List("one","two") >>= ((s: String) => List("number " ++ s))

sealed trait MyList[+A]
case object Nil extends MyList
case class Cons[A](a: A, as: MyList[A]) extends MyList[A]

@main
def main: Unit =
  import PrintableInstances._
  import PrintableSyntax._
  // import EqualSyntax._
  Printable.print("sajt")
  Printable.print(12)
  Printable.print(Cat("jani", 12, "black"))
  println(Printable.format(Cat("jani", 12, "black")))
  Cat("jani", 12, "black").print
  println(Cat("jani", 12, "black").show)
  val showInt: Show[Int] = Show.apply
  println(showInt.show(123))
  println(Show[Int].show(123))
  println(123.show)
  println(List(1,2,3).show)
  println(sajt.show)
  println(1 === 2)
  println(1 =!= 2)
  println(Equal[Int].eqv(1,1))
  println(Option(Cat("jani", 12, "black")) === Option.empty[Cat])
  println(Cons(1, Cons(2, Nil)))

  println(Monoid[Int].combine(2,3))
  println(2.combine(3))
  println(2 |+| 3)

  println(Option.empty |+| Option(3))
  println("jdfls" |+| "fjdsl")
