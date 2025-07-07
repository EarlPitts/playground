package HTTP.Server

import parsley.combinator.*
import parsley.character.*
import parsley.Parsley
import parsley.extension.HaskellStyleMap
import cats.*
import cats.implicits.*
import cats.effect.*
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.*

import java.net.ServerSocket
import java.io.InputStream

object App extends IOApp.Simple:

  implicit val logger: SelfAwareStructuredLogger[IO] = Slf4jLogger.getLogger[IO]

  def run: IO[Unit] =

    val server = Resource.make(IO(ServerSocket(1234)))(s => IO(s.close))
    server.use(s => (IO.interruptible(s.accept) >> IO.println("accepted")).foreverM)
    // Resource
    //   .make(IO(ServerSocket(8982)))(s => IO(s.close()))
    //   .use(serveRequest(_).foreverM)

val request =
  """GET / HTTP/1.1
Host: google.com
User-Agent: curl/8.8.0
Accept: application/json, */*

"""

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
      case Ok(code) => s"${r.version} $code OK\r\n\r\n${r.body}"

lazy val p: Parsley[Request] = for {
  mode <- pMode
  _ <- space
  path <- pPath
  _ <- space
  version <- pVersion
  _ <- endOfLine
  headers <- pHeaders
  _ <- endOfLine
} yield Request(mode, path, version, headers)

lazy val pMode: Parsley[Mode] =
  string("GET") <|>
    string("POST") <|>
    string("PUT") <|>
    string("DELETE") map
    Mode.apply

lazy val pPath: Parsley[String] = stringOfMany(noneOf(' '))
lazy val pVersion: Parsley[String] =
  string("HTTP") *> char('/') *> stringOfMany(noneOf('\r'))
lazy val pHeaders: Parsley[Map[String, String]] =
  sepEndBy(pHeader, endOfLine) map Map.from
lazy val pHeader: Parsley[(String, String)] =
  stringOfMany(noneOf(':', '\r')) >>= ((key: String) =>
    string(": ") *> stringOfMany(noneOf('\r')) map ((key, _))
  )

val r = p.parse(request).getOrElse(Request(GET, "/", "", Map()))

def loadFile(path: String): String =
  val fileName = if path.endsWith("/") then "index.html" else path
  scala.io.Source.fromFile(fileName).mkString

def createResponse(req: Request): Response =
  val version = "HTTP/" ++ req.version
  val body = loadFile(req.path)
  Response(version, Ok(200), body)

def readRequestBytes[F[_]: Sync: Logger](in: InputStream): F[List[Char]] =
  def go(acc: List[Int]): F[List[Int]] = for
    b <- Sync[F].blocking(in.read)
    // _ <- Logger[F].info(b.show)
    bytes <-
      if b == 10 && acc.length > 2 && acc.take(3) == List(13, 10, 13) then
        (b :: acc).pure[F]
      else go(b :: acc)
  yield bytes
  go(List.empty[Int]).map(_.reverse.map(_.toChar))

// serveRequest(r).show

def errorResponse(e: String): Response =
  Response("HTTP/1.1", Ok(200), e)

def serveRequest[F[_]: Logger: Sync](server: ServerSocket): F[Unit] = for
  soc <- Sync[F].blocking(server.accept)
  _ <- Logger[F].info("connection accepted")
  out = soc.getOutputStream
  in = soc.getInputStream
  in <- readRequestBytes(in)
    .flatTap(chars => Logger[F].info(chars.mkString))
    .map(_.mkString)
  _ <- Logger[F].info(s"input: $in")
  req = p.parse(in)
  _ <- Logger[F].info(s"request parsed: $req")
  resp = req.fold((e => errorResponse(e)), (req => createResponse(req)))
  _ <- Logger[F].info(
    s"response created: ${resp.version.toList} ${resp.status}"
  )
  _ <- Sync[F].blocking(out.write(resp.show.getBytes))
  _ <- Sync[F].blocking(out.close)
  _ <- Logger[F].info(s"send: ${resp.show}")
yield ()
