package home.textboard

import cats.effect.*
import cats.*
import cats.implicits.*
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.circe.*
import org.http4s.EntityDecoder
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.circe.*
import io.circe.generic.auto.*
import Posts.PostText
import Posts.PostText.*

object Routes:
  val dsl = new Http4sDsl[IO] {}
  import dsl.*

  def createPost(postRepo: PostRepo): HttpRoutes[IO] =
    HttpRoutes.of[IO]:
      case GET -> Root / "post" / text =>
        Write.fromDTO(Write.PostDTO(text)) match
          case None       => BadRequest()
          case Some(post) => postRepo.createPost(post).flatMap(Ok(_))
