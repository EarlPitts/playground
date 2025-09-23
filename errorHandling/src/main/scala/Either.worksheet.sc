import cats.*
import cats.implicits.*
import cats.data.*
import cats.effect.*
import cats.effect.implicits.*
import UserErr.*

final case class User(
    id: Int,
    age: Int,
    name: String,
    address: String
)

enum UserErr:
  case NotFound
  case AlreadyInDB
  case InvalidAge
  case InvalidName

final case class UserRepository[F[_]: Applicative]():
  def add(
      age: Int,
      name: String,
      address: String
  ): F[Either[UserErr, Int]] =
    if name == "Janos"
    then Left(AlreadyInDB).pure
    // then AlreadyInDB.raiseError.pure // Also works, Either has an ApplicativeError instance
    else Right(1).pure

trait UserService[F[_]]:
  def createUser(
      age: Int,
      name: String,
      address: String
  ): F[Either[UserErr, User]]

object UserService:
  def mkUserService[F[_]: Monad](
      userRepository: UserRepository[F]
  ): UserService[F] = new UserService[F]:
    // Problem: Monads don't compose well
    def createUser(
        age: Int,
        name: String,
        address: String
    ): F[Either[UserErr, User]] =
      validateAge(age) match
        case Left(e) => Left(e).pure
        case Right(()) =>
          validateName(name) match
            case Left(e) => Left(e).pure
            case Right(()) =>
              userRepository
                .add(age, name, address)
                .map {
                  case Left(e) => Left(e)
                  case Right(id) =>
                    Right(User(id, age, name, address))
                }

    private def validateAge(
        age: Int
    ): Either[UserErr, Unit] =
      if age > 100 || age < 0
      then Left(InvalidAge)
      else Right(())

    private def validateName(
        name: String
    ): Either[UserErr, Unit] =
      if name.size > 10
      then Left(InvalidName)
      else Right(())

val service =
  UserService.mkUserService[Id](UserRepository())

service
  .createUser(87, "Imre", "utca utca")
// .handleError { case InvalidName =>
//   User(2, 88, "Bela", "utca ut")
// }
