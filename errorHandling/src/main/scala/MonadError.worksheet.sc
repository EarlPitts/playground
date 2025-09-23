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

type MonadUserErr[F[_]] = MonadError[F, UserErr]

final case class UserRepository[F[_]: MonadUserErr]():
  def add(
      age: Int,
      name: String,
      address: String
  ): F[Int] =
    if name == "Janos"
    then AlreadyInDB.raiseError
    else 1.pure

trait UserService[F[_]]:
  def createUser(
      age: Int,
      name: String,
      address: String
  ): F[User]

object UserService:
  def mkUserService[F[_]: MonadUserErr](
      userRepository: UserRepository[F]
  ): UserService[F] = new UserService[F]:
    def createUser(
        age: Int,
        name: String,
        address: String
    ): F[User] =
      for {
        _ <- validateAge(age)
        _ <- validateName(name)
        id <- userRepository.add(age, name, address)
      } yield User(id, age, name, address)

    private def validateAge(
        age: Int
    ): F[Unit] =
      if age > 100 || age < 0
      then InvalidAge.raiseError
      else ().pure

    private def validateName(
        name: String
    ): F[Unit] =
      if name.size > 10
      then InvalidName.raiseError
      else ().pure

type EitherUserErr[A] = Either[UserErr, A]

val service =
  UserService.mkUserService[EitherUserErr](UserRepository())

service
  .createUser(87, "Imre", "utca utca")
  .handleError {
    case InvalidName => User(2, 88, "Bela", "utca ut")
  }
