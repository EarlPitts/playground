package home.textboard

import cats.effect.Concurrent
import cats.implicits.*
import io.circe.{Encoder, Decoder}
import org.http4s.*
import org.http4s.circe.*
import doobie.util.transactor.Transactor
import doobie.implicits.*

trait Posts[F[_]]:
  def addPost(text: Posts.PostText): F[Unit]
  def getPosts: F[List[Posts.Post]]

object Posts:

  final case class Post(id: Int, text: PostText)
  object Post:
    given Decoder[Post] = Decoder.derived[Post]
    given [F[_]: Concurrent]: EntityDecoder[F, Post] = jsonOf
    given Encoder[Post] = Encoder.AsObject.derived[Post]
    given [F[_]]: EntityEncoder[F, Post] = jsonEncoderOf
    given [F[_]]: EntityEncoder[F, List[Post]] = jsonEncoderOf

  final case class PostText(text: String)

  object PostText:
    given Decoder[PostText] = Decoder.derived[PostText]
    given [F[_]: Concurrent]: EntityDecoder[F, PostText] = jsonOf
    given Encoder[PostText] = Encoder[String].contramap[PostText](_.text)
    given [F[_]]: EntityEncoder[F, PostText] = jsonEncoderOf

  final case class PostsError(e: Throwable) extends RuntimeException

  def mkPosts[F[_]: Concurrent](xa: Transactor[F]): Posts[F] = new Posts[F]:
    def addPost(text: PostText): F[Unit] =
      sql"insert into post (text) values ($text)".update.run
        .transact(xa)
        .void

    def getPosts: F[List[Post]] =
      sql"select * from post"
        .query[Post]
        .to[List]
        .transact(xa)
