import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import cats.Monad
import cats.implicits.*

case class User(id: Long, name: String)
case class Company(id: Long, name: String)

object Example:
  type Effect[A] = OptionT[Future, A]

  def findUserById(id: Long): Effect[User] = ???

  def findCompanyByUser(user: User): Effect[Company] = ???

  def findCompanyByUserId(id: Long)(using ExecutionContext): Effect[Company] =
    for
      user <- findUserById(id)
      company <- findCompanyByUser(user)
    yield company

  case class OptionT[F[_]: Monad, A](value: F[Option[A]]):
    def map[B](f: A => B)(using ExecutionContext): OptionT[F, B] =
      OptionT(value.map(_.map(f)))

    def flatMap[B](
        f: A => OptionT[F, B]
    )(using ExecutionContext): OptionT[F, B] =
      OptionT(value.flatMap {
        case Some(res) => f(res).value
        case None      => None.pure
      })
