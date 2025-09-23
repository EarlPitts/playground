import cats.*
import cats.implicits.*
import cats.data.*
import cats.data.Validated.*
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
  ): F[Validated[NonEmptyList[UserErr], Int]] =
    if name == "Janos"
    then Invalid(NonEmptyList.of(AlreadyInDB)).pure
    else Valid(1).pure

trait UserService[F[_]]:
  def createUser(
      age: Int,
      name: String,
      address: String
  ): F[Validated[NonEmptyList[UserErr], User]]

object UserService:
  def mkUserService[F[_]: Applicative](
      userRepository: UserRepository[F]
  ): UserService[F] = new UserService[F]:
    def createUser(
        age: Int,
        name: String,
        address: String
    ): F[Validated[NonEmptyList[UserErr], User]] =
      (validateAge(age), validateName(name)).tupled match
        case Invalid(es) => Invalid(es).pure
        case Valid((age, name)) =>
          userRepository
            .add(age, name, address)
            .map(_.map(User(_, age, name, address)))

    private def validateAge(
        age: Int
    ): Validated[NonEmptyList[UserErr], Int] =
      if age > 100 || age < 0
      then Invalid(NonEmptyList.of(InvalidAge))
      else Valid(age)

    private def validateName(
        name: String
    ): Validated[NonEmptyList[UserErr], String] =
      if name.size > 10
      then Invalid(NonEmptyList.of(InvalidName))
      else Valid(name)

val service =
  UserService.mkUserService[Id](UserRepository())

service.createUser(
  12,
  "Janos",
  "ujpest"
)
