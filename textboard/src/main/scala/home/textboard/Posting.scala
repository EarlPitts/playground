package home.textboard

import cats.effect.Concurrent
import cats.implicits.*
import io.circe.{Encoder, Decoder}
import home.textboard.Posts.*

class Posting[F[_]](
    posts: Posts[F]
) {
  def createPost(text: PostText): F[Unit] =
    posts.addPost(text)

  def getPosts: F[List[Post]] =
    posts.getPosts
}
