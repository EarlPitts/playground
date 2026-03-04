package home.textboard

import cats.effect.*
import cats.*
import cats.implicits.*
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.circe.*
import org.http4s.EntityDecoder
import Posts.PostText
import Posts.PostText.*

object TextboardRoutes:

  def jokeRoutes[F[_]: Sync](J: Jokes[F]): HttpRoutes[F] =
    val dsl = new Http4sDsl[F] {}
    import dsl.*
    HttpRoutes.of[F] { case GET -> Root / "joke" =>
      for {
        joke <- J.get
        resp <- Ok(joke)
      } yield resp
    }

  def helloWorldRoutes[F[_]: Sync](H: HelloWorld[F]): HttpRoutes[F] =
    val dsl = new Http4sDsl[F] {}
    import dsl.*
    HttpRoutes.of[F] { case GET -> Root / "hello" / name =>
      for {
        greeting <- H.hello(HelloWorld.Name(name))
        resp <- Ok(greeting)
      } yield resp
    }

  def postingRoutes[F[_]: Concurrent: MonadThrow](
      p: Posting[F]
  ): HttpRoutes[F] =
    val dsl = new Http4sDsl[F] {}
    import dsl.*
    HttpRoutes.of[F] { case req @ POST -> Root / "posts" / ize =>
      // req.as[PostText].flatMap(t => Ok(t.text))
      // p.createPost(PostText(ize)).as(Ok(ize)).flatten
      req
        .as[PostText]
        .flatMap(t => p.createPost(t))
        .flatMap(_ => Ok())
    }
