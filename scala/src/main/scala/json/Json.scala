package JsonAPI

import cats.implicits._, cats._, cats.derived._

enum Json:
  case JsonObject(m: Map[String, Json])
  case JsonString(s: String)
  case JsonNum(n: Int)
  case JsonList(l: List[Json])

object Json:
  given Show[Json] with
    def show(j: Json): String = j match
      case JsonObject(m: Map[String, Json]) => m.map((s,j) => s"\"s\": ${j.show}").mkString("{", ",", "}")
      case JsonNum(n: Int)                  => s"$n"
      case JsonString(s: String)            => s"\"$s\""
      case JsonList(l: List[Json])          => l.map(_.show).mkString("[", ",", "]")

trait Encodable[A]:
  def toJson(a: A): Json

object Encodable:
  def apply[A](implicit instance: Encodable[A]): Encodable[A] = instance
