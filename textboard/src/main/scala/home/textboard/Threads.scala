package home.textboard

import cats.effect.Concurrent
import cats.implicits.*
import io.circe.{Encoder, Decoder}
import Posts.Post

trait Threads[F[_]]:
  def getThreads: F[List[Threads.Thread]]

object Threads:

  final case class Thread(id: Int, posts: List[Post])
  // object Thread:
  //   given Decoder[Joke] = Decoder.derived[Joke]
  //   given [F[_]: Concurrent]: EntityDecoder[F, Joke] = jsonOf
  //   given Encoder[Joke] = Encoder.AsObject.derived[Joke]
  //   given [F[_]]: EntityEncoder[F, Joke] = jsonEncoderOf

  final case class ThreadError(e: Throwable) extends RuntimeException

  def mkThreads[F[_]: Concurrent]: Threads[F] = new Threads[F]:
    def getThreads: F[List[Thread]] = ???
