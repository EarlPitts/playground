package Web

import cats._
import cats.implicits._
import cats.effect._

import com.comcast.ip4s._

import io.circe._
import io.circe.literal._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.ember.server.EmberServerBuilder


case class Hello(greeting: String)
case class User(name: String)

object User:
  import io.circe.generic.auto._
  import org.http4s.circe._

  implicit val decoder: EntityDecoder[IO, User] = jsonOf[IO, User]

object Server extends IOApp.Simple:
  import org.http4s.circe._
  import io.circe.syntax._
  import io.circe.generic.auto._

  val jsonApp = HttpRoutes.of[IO] {
    case req @ POST -> Root / "hello" =>
      for
        user <- req.as[User]
        resp <- Ok(Hello(user.name).asJson)
      yield resp
  }.orNotFound

  val server = EmberServerBuilder
    .default[IO]
    .withHost(ipv4"127.0.0.1")
    .withPort(port"8080")
    .withHttpApp(jsonApp)
    .build

  def run: IO[Unit] =
    server.use(_ => IO.never)

object Client extends IOApp.Simple:
  import org.http4s.ember.client._
  import org.http4s.circe._
  import io.circe.generic.auto._
  import io.circe.syntax._

  def helloClient(name: String): IO[Hello] =
    val req = Request[IO](Method.POST, uri"http://localhost:8080/hello")
      .withEntity(User(name).asJson)

    EmberClientBuilder.default[IO].build.use { client =>
      client.expect(req)(jsonOf[IO, Hello])
    }

  def run: IO[Unit] =
    helloClient("Bob") >>= IO.println

object Jsoning extends IOApp.Simple:
  def hello(name: String): Json =
    json"""{"hello": $name}"""

  import org.http4s.circe._ // This is the magic sauce

  def run: IO[Unit] =
    Ok(hello("jozsi")) >>= IO.println
