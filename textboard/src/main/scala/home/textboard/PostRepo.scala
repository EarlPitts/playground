package home.textboard

import cats.effect.*
import doobie.util.transactor.Transactor
import doobie.implicits.*

trait PostRepo:
  def createPost(post: Write.Post): IO[Read.Post]

object PostRepo:
  def mkRepo(xa: Transactor[IO]): PostRepo =
    new PostRepo:
      def createPost(post: Write.Post): IO[Read.Post] =
        sql"insert into post (text) values (${post.text})".update
          .withUniqueGeneratedKeys[Int]("id")
          .transact(xa)
          .map(id => Read.Post(id, post.text))
