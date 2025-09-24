package ErrorHandling.MonadThrow

import cats.*
import cats.implicits.*
import cats.data.*
import cats.effect.*
import cats.effect.implicits.*
import UserThrowable.*

final case class User(
    id: Int,
    age: Int,
    name: String,
    address: String
)

enum UserThrowable extends Throwable:
  case NotFound
  case AlreadyInDB
  case InvalidAge
  case InvalidName

final case class UserRepository[F[_]: MonadThrow]():
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
  def mkUserService[F[_]: MonadThrow](
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

type EitherThrow[A] = Either[Throwable, A]

val service =
  UserService.mkUserService[EitherThrow](UserRepository())

// Problem: as IO has Throwable hard-coded into it,
// you cannot have an instance with your own error type

def makeUser(age: Int, name: String, address: String) =
  service
    .createUser(age, name, address)
    .handleError {
      case InvalidName => User(1, 2, "3", "4")
    }
    // .onError {
    //   case InvalidName => Right(println("invalid name"))
    //   case InvalidAge  => Right(println("invalid age"))
    // }
