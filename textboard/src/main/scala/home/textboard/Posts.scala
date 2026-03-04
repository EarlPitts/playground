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

object Posts:

  final case class Post(id: Int, text: PostText)

  final case class PostText(text: String)

  object PostText:
    given Decoder[PostText] = Decoder.derived[PostText]
    given [F[_]: Concurrent]: EntityDecoder[F, PostText] = jsonOf

  final case class PostsError(e: Throwable) extends RuntimeException

  def mkPosts[F[_]: Concurrent](xa: Transactor[F]): Posts[F] = new Posts[F]:
    def addPost(text: PostText): F[Unit] =
      sql"insert into post (text) values ($text)".update.run
        .transact(xa)
        .void
