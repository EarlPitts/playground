package felines.Cats

import cats.*
import cats.Applicative
import cats.data.*
import cats.syntax.all.*

def test(oa: Option[Int], ob: Option[Int]): Option[Int] =
  for
    a <- oa
    b <- ob
    c = a + b
  yield c

// def liftA2[F[_],A,B](f: A => B)(fa: F[A]): F[B] =
//   Applicative.pure(f).map(fa)

val _ = Applicative[Option].pure((x: Int) => (x + 1)) <*> Some(2)
val a = Applicative[Option].map2(Some(2),Some(3))((x: Int, y: Int) => (x + y)) // This is like LiftA2

val b = Applicative[Option].product(Some(2), Some(3))
val c = Applicative[List].product(List(1,2,3),List(4,5,6))
val d = Applicative[Option].ap2(Some((x:Int, y: Int) => (x + y)))(Some(2), Some(3))
val e = Some(2).map((x: Int) => (y: Int) => x + y) <*> Some(3)

val f = Some(2).flatMap(x => Some(x + 2))

object ApplyTypeclass:
  implicit val optionApply: Apply[Option] = new Apply[Option] {
    def ap[A, B](f: Option[A => B])(fa: Option[A]): Option[B] =
      fa.flatMap(a => f.map(f => f(a)))

    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f

    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A,B)] =
      fa.flatMap(a => fb.map(b => (a,b)))
  }

  implicit val listApply: Apply[List] = new Apply[List] {
    def ap[A, B](f: List[A => B])(fa: List[A]): List[B] =
      f.flatMap(fun => fa.map(fun))

    def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f

    override def product[A, B](fa: List[A], fb: List[B]): List[(A,B)] =
      fa.flatMap(a => fb.map(b => (a,b)))
  }
