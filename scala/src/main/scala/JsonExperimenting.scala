package JsonExperimenting

import cats.implicits._, cats._, cats.derived._

import JsonAPI.*
import JsonAPI.Json.*

enum Color:
  case Black
  case Beige
  case White
import Color.*

object Color:
  given Show[Color] with
    def show(c: Color) = c.toString()

  given Encodable[Color] with
    def toJson(c: Color) = JsonString(c.show)

case class Dog(name: String, age: Int, color: List[Color]) derives Show, Eq

object Dog:
  given Encodable[Dog] with
    def toJson(d: Dog) = d match
      case Dog(name, age, color) =>
        val map = Map(
          "name" -> JsonString(name),
          "age" -> JsonNum(age),
          "color" -> JsonList(color.map(Encodable[Color].toJson(_)))
        )
        JsonObject(map)

extension (d: Dog)
  def toJson = Encodable[Dog].toJson(d)

val d = Dog("Bloki", 4, List(White, Black))
val d2 = Dog("Donci", 2, List(Beige, Black))
