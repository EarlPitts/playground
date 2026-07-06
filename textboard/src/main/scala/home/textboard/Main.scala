package home.textboard

import cats.effect.*

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
import doobie.hikari.HikariTransactor
import scala.concurrent.ExecutionContext

object Main extends IOApp.Simple:
  def run =
    HikariTransactor
      .newHikariTransactor[IO](
        "org.sqlite.JDBC",
        "jdbc:sqlite:mydb.sqlite",
        "",
        "",
        ExecutionContext.global
      ).use { xa =>
        
        val postRepo = PostRepo.mkRepo(xa)
        val httpApp = Routes.createPost(postRepo).orNotFound

        EmberServerBuilder
          .default[IO]
          .withHost(ipv4"0.0.0.0")
          .withPort(port"8080")
          .withHttpApp(httpApp)
          .build
          .useForever
      }

