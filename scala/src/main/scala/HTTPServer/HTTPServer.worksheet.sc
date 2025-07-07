import parsley.*
import parsley.combinator.*
import parsley.character.*
import parsley.Parsley
import parsley.extension.HaskellStyleMap
import cats.*
import cats.implicits.*
import cats.effect.*

import java.net.Socket
import scala.io.Source

val request = Source.fromFile("sajt").mkString

// val request =
// """GET / HTTP/1.1
// Host: google.com
// User-Agent: curl/8.8.0
// Accept: application/json, */*
//
// """

sealed trait Mode
case object GET extends Mode

object Mode:
  def apply(modeStr: String): Mode = modeStr match
    case "GET"    => GET
    case "POST"   => GET
    case "PUT"    => GET
    case "DELETE" => GET

sealed trait StatusCode
case class Ok(code: Int) extends StatusCode

case class Request(
    mode: Mode,
    path: String,
    version: String,
    headers: Map[String, String]
)

case class Response(version: String, status: StatusCode, body: String)

object Response:
  given Show[Response] with
    def show(r: Response): String = r.status match
      case Ok(code) => s"${r.version} $code OK\r\r${r.body}"

lazy val p: Parsley[Request] = for {
  mode <- pMode
  _ <- space
  path <- pPath
  _ <- space
  version <- pVersion
  _ <- endOfLine
  headers <- pHeaders
  _ <- crlf
} yield Request(mode, path, version, headers)

lazy val pMode: Parsley[Mode] =
  string("GET") <|>
    string("POST") <|>
    string("PUT") <|>
    string("DELETE") map
    Mode.apply

lazy val pPath: Parsley[String] = stringOfMany(noneOf(' '))
lazy val pVersion: Parsley[String] =
  string("HTTP") *> char('/') *> stringOfMany(noneOf('\n'))
lazy val pHeaders: Parsley[Map[String, String]] =
  sepEndBy(pHeader, endOfLine) map Map.from
lazy val pHeader: Parsley[(String, String)] =
  stringOfSome(noneOf(':', '\n')) >>= ((key: String) =>
    string(": ") *> stringOfSome(noneOf('\n')) map ((key, _))
  )

val r = p.parse(request) //.getOrElse(Request(GET, "/", "", Map()))

def loadFile(path: String): String =
  val fileName = if path.endsWith("/") then "index.html" else path
  scala.io.Source.fromFile(fileName).mkString

def serveRequest(req: Request): Response =
  val version = "HTTP/" ++ req.version
  val body = loadFile(req.path)
  Response(version, Ok(200), body)

// serveRequest(r).show

object App extends IOApp.Simple:
  def run: IO[Unit] =
    val socket =
      Resource.make(IO(new Socket("localhost", 8080)))(s => IO(s.close))
    socket.use { soc =>
      val out = soc.getOutputStream
      val in = soc.getInputStream
      val resp = for {
        req <- p.parse(in.readAllBytes.mkString)
        resp = serveRequest(req)
      } yield resp
      resp.fold(
        (e => IO.blocking(out.write(e.getBytes))),
        (resp => IO.blocking(out.write(resp.show.getBytes)))
      )
    }

"kacsa".toList.map(_.toByte).map(_.toChar).mkString


val s = List(13,10,13,10).map(_.toChar).mkString

(endOfLine *> endOfLine).parse(s)
