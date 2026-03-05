package home.textboard

import cats.effect.Async
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.Network
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.middleware.Logger
import org.http4s.server.middleware._
import cats.effect._
import doobie._
import doobie.implicits._

object Database {
  def transactor[F[_]: Async]: Resource[F, Transactor[F]] =
    Resource.pure(
      Transactor.fromDriverManager[F](
        driver = "org.sqlite.JDBC",
        url = "jdbc:sqlite:database.db",
        None
      )
    )
}

object TextboardServer:

  def run[F[_]: Async: Network]: F[Nothing] = {
    for {
      client <- EmberClientBuilder.default[F].build
      helloWorldAlg = HelloWorld.impl[F]
      jokeAlg = Jokes.impl[F](client)

      xa <- Database.transactor

      posts = Posts.mkPosts[F](xa)
      posting = Posting(posts)

      // Combine Service Routes into an HttpApp.
      // Can also be done via a Router if you
      // want to extract segments not checked
      // in the underlying routes.
      httpApp = CORS.policy.withAllowOriginAll(
        TextboardRoutes.postingRoutes[F](posting) <+>
          TextboardRoutes.helloWorldRoutes[F](helloWorldAlg) <+>
          TextboardRoutes.jokeRoutes[F](jokeAlg)
      ).orNotFound

      // With Middlewares in place
      finalHttpApp = Logger.httpApp(true, true)(httpApp)

      _ <-
        EmberServerBuilder
          .default[F]
          .withHost(ipv4"0.0.0.0")
          .withPort(port"8080")
          .withHttpApp(finalHttpApp)
          .build
    } yield ()
  }.useForever
