package Validation

import cats._
import cats.implicits._
import cats.data.Validated

object MyApproach {

  trait ValidationError
  case object NotLongEnough extends ValidationError
  case object NoLowercase extends ValidationError
  case object NoUppercase extends ValidationError

  def validLength(pass: String): Validated[List[ValidationError], String] =
    if pass.size > 10 then Validated.Valid(pass)
    else Validated.Invalid(List(NotLongEnough))
  def containsLower(pass: String): Validated[List[ValidationError], String] =
    if pass.exists(_.isLower) then Validated.Valid(pass)
    else Validated.Invalid(List(NoLowercase))
  def containsUpper(pass: String): Validated[List[ValidationError], String] =
    if pass.exists(_.isUpper) then Validated.Valid(pass)
    else Validated.Invalid(List(NoUppercase))

  def validatePassword(pass: String): Validated[List[ValidationError], String] =
    validLength(pass) *> containsLower(pass) *> containsUpper(pass)

  println(validatePassword("sajt"))
  println(validatePassword("Sajtoskiflisbanan"))
}

object CheckAsFunc {
  case class CheckF[E: Semigroup, A](f: A => Either[E, A]) {
    def apply(a: A) = f(a)

    def and(that: CheckF[E, A]): CheckF[E, A] = CheckF { a =>
      (this(a), that(a)) match
        case (Right(_), Right(_)) => a.asRight
        case (Left(e1), Left(e2)) => e1.combine(e2).asLeft
        case (Left(e), _)         => e.asLeft
        case (_, Left(e))         => e.asLeft
    }
  }

  val tooBig = CheckF[List[String], Int] { n =>
    if (n > 5) List("too big").asLeft else n.asRight
  }
  val tooSmall = CheckF[List[String], Int] { n =>
    if (n < 2) List("too small").asLeft else n.asRight
  }
  val notNine = CheckF[List[String], Int] { n =>
    if (n === 9) List("Shouldn't be nine").asLeft
    else n.asRight
  }
  val sizeCheck = tooBig and tooSmall and notNine

  println(sizeCheck(1))
  println(sizeCheck(3))
  println(sizeCheck(7))
  println(sizeCheck(9))

}

object CheckAsADT {
  trait Check[E: Semigroup, A] {
    import Check._

    def and(that: Check[E, A]): Check[E, A] =
      And(this, that)

    def apply(a: A): Either[E, A] =
      this match {
        case Pure(f) =>
          f(a)

        case And(left, right) =>
          (left(a), right(a)) match {
            case (Right(_), Right(_)) => a.asRight
            case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
            case (Left(e), _)         => e.asLeft
            case (_, Left(e))         => e.asLeft
          }
      }
  }

  object Check {
    case class And[E: Semigroup, A](left: Check[E, A], right: Check[E, A])
        extends Check[E, A]
    case class Pure[E: Semigroup, A](f: A => Either[E, A]) extends Check[E, A]

    def pure[E: Semigroup, A](f: A => Either[E, A]): Check[E, A] = Pure(f)
  }

  val tooBig: Check[List[String], Int] = Check.pure { n =>
    if (n > 5) List("too big").asLeft else n.asRight
  }
  val tooSmall: Check[List[String], Int] = Check.pure { n =>
    if (n < 2) List("too small").asLeft else n.asRight
  }
  val notNine: Check[List[String], Int] = Check.pure { n =>
    if (n === 9) List("Shouldn't be nine").asLeft
    else n.asRight
  }
  val sizeCheck = tooBig and tooSmall and notNine

  println(sizeCheck(1))
  println(sizeCheck(3))
  println(sizeCheck(7))
  println(sizeCheck(9))
}

object CheckADTValidated {
  import cats.data.Validated._

  trait Check[E: Semigroup, A] {
    import Check._

    def and(that: Check[E, A]): Check[E, A] =
      And(this, that)

    def or(that: Check[E, A]): Check[E, A] =
      Or(this, that)

    def apply(a: A): Validated[E, A] =
      this match {
        case Pure(f) =>
          f(a)

        case And(left, right) =>
          (left(a), right(a)).mapN((_, _) => a)

        case Or(left, right) =>
          (left(a), right(a)) match
            case (Invalid(e1), Invalid(e2)) => Invalid(e1 |+| e2)
            case _                          => Valid(a)
      }
  }

  object Check {
    case class And[E: Semigroup, A](left: Check[E, A], right: Check[E, A])
        extends Check[E, A]
    case class Or[E: Semigroup, A](left: Check[E, A], right: Check[E, A])
        extends Check[E, A]
    case class Pure[E: Semigroup, A](f: A => Validated[E, A])
        extends Check[E, A]

    def pure[E: Semigroup, A](f: A => Validated[E, A]): Check[E, A] = Pure(f)
  }

  val tooBig: Check[List[String], Int] = Check.pure { n =>
    if (n > 5) Invalid(List("too big")) else Valid(n)
  }
  val tooSmall: Check[List[String], Int] = Check.pure { n =>
    if (n < 2) Invalid(List("too small")) else Valid(n)
  }
  val notNine: Check[List[String], Int] = Check.pure { n =>
    if (n === 9) Invalid(List("Shouldn't be nine"))
    else Valid(n)
  }
  val sizeCheck = tooBig and tooSmall and notNine
  val sizeCheckOr = tooBig or notNine

  println(sizeCheckOr(1))
  println(sizeCheckOr(3))
  println(sizeCheckOr(7))
  println(sizeCheckOr(9))
}

object TransformingData {
  import cats.data.Validated._

  sealed trait Predicate[E, A] {
    def and(that: Predicate[E, A]): Predicate[E, A] =
      And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] =
      Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) =>
          func(a)

        case And(left, right) =>
          (left(a), right(a)).mapN((_, _) => a)

        case Or(left, right) =>
          (left(a), right(a)) match
            case (Invalid(e1), Invalid(e2)) => Invalid(e1 |+| e2)
            case _                          => Valid(a)
      }
  }

  final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A])
      extends Predicate[E, A]

  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A])
      extends Predicate[E, A]

  final case class Pure[E, A](func: A => Validated[E, A])
      extends Predicate[E, A]

  sealed trait Check[E, A, B] {
    import Check._

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

    def map[C](f: B => C): Check[E, A, C] =
      Map[E, A, B, C](this, f)

    def flatMap[C](f: B => Check[E, A, C]) =
      FlatMap[E, A, B, C](this, f)

    def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
      AndThen[E, A, B, C](this, that)
  }

  object Check {
    final case class Map[E, A, B, C](check: Check[E, A, B], f: B => C)
        extends Check[E, A, C] {
      def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
        check(in).map(f)
    }

    final case class FlatMap[E, A, B, C](
        check: Check[E, A, B],
        f: B => Check[E, A, C]
    ) extends Check[E, A, C] {
      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
        check(a).withEither(_.flatMap(b => f(b)(a).toEither))
    }

    final case class AndThen[E, A, B, C](c1: Check[E, A, B], c2: Check[E, B, C])
        extends Check[E, A, C] {
      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
        c1(a).withEither(_.flatMap(b => c2(b).toEither))
    }

    final case class Pure[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {

      def apply(in: A)(implicit s: Semigroup[E]): Validated[E, A] =
        pred(in)
    }

    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
      Pure(pred)
  }

}

@main
def main: Unit =
  // MyApproach
  // CheckAsFunc
  // CheckAsADT
  // CheckADTValidated
  TransformingData
