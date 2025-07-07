// trait Ize
//
// case object Bize extends Ize
// case object Hoze extends Ize
//
// case class Malac(x: Int, y: Int)
//
// val y: Ize = Bize
//
// type M[A] = Either[A, A]
//
// val x: M[Int] = Left(2)

import cats.*
import cats.implicits.*


// TODO Filter out Nones with GADTs, so the compiler knows that the list cannot contain None values?

object WithADT:
  case class UserName(firstName: String, lastName: String)
  case class UserID(userID: Int)
  enum User:
    case UserByName(name: UserName)
    case UserByID(id: UserID)
  import User.*

  def usersWithFirstName(targetName: String, users: List[User]): List[User] =
    val matchingFirstName: User => Boolean = nameRecord => nameRecord match
      case UserByName(UserName(first, last)) => first == targetName
      case _ => false
    users.filter(matchingFirstName)

  val users = List(UserByID(UserID(123)), UserByName(UserName("Lajos", "Kiss")))
  usersWithFirstName("Lajos", users)


trait UserByName
trait UserByID
enum User[A]:
  case UserName(firstName: String, lastName: String) extends User[UserByName]
  case UserID(userID: Int) extends User[UserByID]
import User.*

// def usersWithFirstName(targetName: String, users: List[User[UserByName]]): List[User[UserByName]] = users.filter(user => user.firstName == targetName)

// val users = List(UserByID(UserID(123)), UserByName(UserName("Lajos", "Kiss")))
// usersWithFirstName("Lajos", users)


// data Ize = Ize Int | Bigyo Int
// case class Ize(x: Int)

// trait Ize
// case class Izee(x: Int) extends Ize
// case class Bigyo(x: Int) extends Ize

enum Ize[A]:
  case Izee extends Ize[String]
  case Bigyo extends Ize[Int]
import Ize.*

Izee
Bigyo
// trait Kecske
//
enum Box[T](contents: T):
  case IntBox(n: Int) extends Box[Int](n)
  case BoolBox(b: Boolean) extends Box[Boolean](b)
import Box.*


def extract[T](b: Box[T]): T = b match
  case IntBox(n)  => n + 1
  case BoolBox(b) => !b

val x: Box[Int] = IntBox(3)
extract(x)

case class Product(x: Int, y: Int)

enum Sum:
  case LeftInj(x: Int)
  case RightInt(y: Int)
import Sum.*

LeftInj(3)

type X = Integer => Integer
