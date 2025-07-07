object FlexibleFind:
  enum T[+A]:
    case Raise
    case ReturnNone
    case Default(a: A)
  import T.*

  def flexibleFind[A](
      as: List[A],
      f: A => Boolean,
      ifNotFound: T[A]
  ): Option[A] = as match
    case a :: as => if f(a) then Some(a) else flexibleFind(as, f, ifNotFound)
    case Nil =>
      ifNotFound match
        case Raise      => throw new RuntimeException("hujuj")
        case ReturnNone => None
        case Default(a) => Some(a)

// import FlexibleFind.*
//
// flexibleFind(List(1, 2, 3), _ >= 4, T.Default(4))

object GADTFlexibleFind:
  enum T[A, B]:
    case Raise[A]() extends T[A, A]
    case ReturnNone[A]() extends T[A, Option[A]]
    case Default(a: A) extends T[A, A]
  import T.*

  // sealed trait T[A, B]
  // case class Raise[A]() extends T[A, A]
  // case class ReturnNone[A]() extends T[A, Option[A]]
  // case class Default[A](a: A) extends T[A, A]

  def flexibleFind[A, B](as: List[A], f: A => Boolean, ifNotFound: T[A, B]): B =
    as match
      case a :: as =>
        if f(a) then
          ifNotFound match
            case Default(_)   => a
            case Raise()      => a
            case ReturnNone() => Some(a)
        else flexibleFind(as, f, ifNotFound)
      case Nil =>
        ifNotFound match
          case Raise()      => throw new RuntimeException("hujuj")
          case ReturnNone() => None
          case Default(a)   => a

import GADTFlexibleFind.*

val ret = flexibleFind(
  List(1, 2, 3),
  _ >= 4,
  T.ReturnNone()
)
