package Functors

import cats._
import cats.implicits._

object impl {
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class FunctorOps[F[_], A](src: F[A]) {
    def map[B](f: A => B)(implicit functor: Functor[F]): F[B] =
      functor.map(src)(f)
  }

  implicit val optionFunctor: Functor[Option] =
    new Functor[Option] {
      def map[A,B](fa: Option[A])(f: A => B): Option[B] = fa match
        case Some(x) => Some(f(x))
        case None    => None
    }
}

def doMath[F[_]: Functor](start: F[Int]): F[Int] =
  start.map(n => (n + 1) * 2)

object TreeFunctor {
  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)

    def leaf[A](value: A): Tree[A] =
      Leaf(value)
  }

  given Functor[Tree] with
    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
      case Leaf(a) => Leaf(f(a))
}

object ContravariantFunctor {
  trait Printable[A] { self =>
    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] =
      new Printable[B] {
        def format(value: B): String =
          self.format(func(value))
      }
  }

  final case class Box[A](value: A)

  object Printable {
    def apply[A](implicit instance: Printable[A]) = instance

    given Printable[String] with
      def format(a: String) = s"'$a'"

    given Printable[Boolean] with
      def format(a: Boolean): String = if a then "yes" else "no"

    implicit def boxPrintable[A](implicit instance: Printable[A]): Printable[Box[A]] =
      instance.contramap(_.value)
  }

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  // You can create typeclass instances "on-the-fly" with contramap for types
  // you don't already have an instance for, but have a homomorphism to some other
  // type thay has an instance
  def x = 
    Show[String].contramap((sym: Symbol) => s"${sym.name}").show(Symbol("sajt"))
}

@main
def main: Unit =
  println(Functor[List].map(List(1,2,3))(_ === 3))
  println(Option(3).map(_ * 2))
  val func = Functor[Option].lift((_: Int) * 2)
  println(func(Option(2)))
  println(doMath(List(1,2,3)))
  println(doMath(Option(3)))
  println(doMath(Either.right(3)))

  import TreeFunctor._
  import TreeFunctor.Tree._
  val t = branch(branch(leaf(2), leaf(4)), branch(leaf(1),leaf(3)))
  println(Functor[Tree].map(t)(_ + 2))
  println(t.map(_ + 2))

  import ContravariantFunctor._

  println(format("sajt"))

  println(Show[String].contramap((sym: Symbol) => s"${sym.name}").show(Symbol("sajt")))
