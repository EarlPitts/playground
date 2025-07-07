import cats._
import cats.implicits._

trait Applicative[F[_]] extends Functor[F]:
  def ap[A, B](fab: F[A => B], fa: F[A]): F[B]

object Applicative:
  def apply[F[_]](implicit instance: Applicative[F]): Applicative[F] = instance

sealed abstract trait Maybe[+A]
case object Nothing extends Maybe[Nothing]
case class Just[A](value: A) extends Maybe[A]

object Maybe:
  implicit def functorInstance: Functor[Maybe] =
    new Functor[Maybe] {
      def map[A, B](fa: Maybe[A])(f: A => B): Maybe[B] = fa match
        case Nothing => Nothing
        case Just(a) => Just(f(a))
    }

  implicit def applicativeInstance: Applicative[Maybe] =
    new Applicative[Maybe] {
      def map[A, B](fa: Maybe[A])(f: A => B): Maybe[B] = fa match
        case Nothing => Nothing
        case Just(a) => Just(f(a))

      def ap[A, B](fab: Maybe[A => B], fa: Maybe[A]): Maybe[B] = (fab, fa) match
        case (Just(f), Just(a)) => Just(f(a))
        case _                  => Nothing
    }


Some(2)
Option(2)
Right(2)
Left(2)
2.asRight[Int]

val a = Just(_ + 1): Maybe[Int => Int]
Applicative[Maybe].map(Just(2))(_ + 1)
Applicative[Maybe].ap(a, Just(2))
