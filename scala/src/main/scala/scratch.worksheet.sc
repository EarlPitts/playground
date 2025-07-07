import java.lang.reflect.Field
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.*
import cats.effect.implicits.*
import cats.implicits.*
import cats.data.Kleisli
import cats.*
import cats.arrow.FunctionK
import cats.effect.std.Dispatcher

def i(n: Int): Int = n
def j(b: Boolean): Int = if b then 0 else 1

def m(e: Either[Int, Boolean]): Int = e match
  case Left(value)  => value
  case Right(value) => if value then 0 else 1

def i2(n: Int): Either[Int, Boolean] = Left(n)
def j2(b: Boolean): Either[Int, Boolean] = Right(b)

(m compose i2)(3)
i2(3)
(m compose j2)(true)
j2(true)

// enum Shape:
//   case Circle(r: Float)
//   case Rect(a: Float, b: Float)
// import Shape.*
//
// def area: (s: Shape) => Float =
//   case Circle(r) => Math.PI.toFloat * r * r
//   case Rect(a, b) => a * b

// sealed trait Shape:
//   def area: Float
//   def circ: Float
//
// case class Circle(r: Float) extends Shape:
//   def area = Math.PI.toFloat * r * r
//   def circ = ???
//
// case class Rect(a: Float, b: Float) extends Shape:
//   def area = a * b
//   def circ = ???
//
// case class Square(a: Float) extends Shape:
//   def area = a * a
//   def circ = ???

sealed trait Shape[S]:
  // def area(s: S): Float
  def circ(s: S): Float
  def area(s: S): Float

object Shape:
  def apply[S](implicit ev: Shape[S]) = ev

  object syntax:
    extension [S: Shape](s: S) def area = Shape[S].area(s)
    extension [S: Shape](s: S) def circ = Shape[S].circ(s)

case class Circle(r: Float)

object Circle:
  given Shape[Circle] with
    def area(c: Circle): Float = Math.PI.toFloat * c.r * c.r
    def circ(c: Circle): Float = Math.PI.toFloat * c.r * 2

case class Rect(a: Float, b: Float)

object Rect:
  given Shape[Rect] with
    def area(r: Rect): Float = r.a * r.b
    def circ(r: Rect): Float = 2 * r.a + 2 * r.b

case class Square(a: Float)
import Shape.syntax.*

Shape[Circle].area(Circle(3))

Circle(2).area

val r = Rect(2, 5)

Shape[Rect].circ(r)
r.circ

// Bifunctor[Tuple2]
//   .compose[Tuple2]
//   .bimap

trait Functor[+A]:
  def map[B](f: A => B): Functor[B]

// trait Maybe[+A] extends Functor[A]
//
// case object Nothing extends Maybe[Nothing]:
//   def map[B](f: scala.Nothing => B): Functor[B] = Nothing
// case class Just[A](value: A) extends Maybe[A]:
//   def map[B](f: A => B): Functor[B] = Just(f(value))

// Just(2).map(_ + 1)
// Nothing.map((x: Int) => x + 2)

type ISet = Int => Boolean

// trait ISet:
//   def empty: Int => ISet
//   def insert: ISet => Int => ISet
//   def union: ISet => ISet => ISet

def empty: ISet = i => false
def insert(s: ISet, n: Int): ISet = i => (i == n || s(i))
def union(s1: ISet, s2: ISet): ISet = i => (s1(i) || s2(i))

val s1 = insert(insert(insert(empty, 3), 4), 5)
val s2 = insert(insert(insert(empty, 1), 4), 8)

given Show[ISet] with
  def show(t: ISet): String =
    List.range(0, 10).filter(t).show

union(s1, s2).show

abstract class Animal:
  def sound: String

class Dog extends Animal:
  def sound = "Bark"

class Spaniel extends Dog:
  override def sound = "Woof"
  def age = 12

def f(d: Dog): Dog = Spaniel()

Either.catchNonFatal(throw new RuntimeException("baj van"))

List({ println(1); 1 }, { println(2); 2 }, { println(3); 3 })
  .foldr[Int](Eval.later(0))((a, b) => b)

import cats.effect._

// Traverse[List].ifF
List(false, true).ifF("false", "true")
List(false, true).ifM("false".pure, "true".pure)


Contravariant[Show]
  .contramap[Int, String](Show[Int])(_.toInt + 1)
  .show("12")

Show[Int]
  .contramap[String](_.length)
  .show("sajt")
  

case class Predicate[A](runPred: A => Boolean)

given Contravariant[Predicate] with
  def contramap[A, B](fa: Predicate[A])(f: B => A): Predicate[B] =
    Predicate[B](b => fa.runPred(f(b)))

val p = Predicate[Int](_ > 10).contramap[String](_.length)

p.runPred("sajtfjdsklajfdsakl")

val ordering = Order[Int]
  .contramap[String](_.length)

List("kutya", "cica", "malac", "kecskebeka").sortWith(ordering.lt)


// trait Identity[A]:
//   def map(
//
//
// import cats.Id
//
// trait Maybe[+A]:
//   def map[B](f: A => B): Maybe[B] = this match
//     case None => None
//     case Some(a) => Some(f(a))
// case object None extends Maybe[Nothing]
// case class Some[A](value: A) extends Maybe[A]
//
// trait Identity[A]:
//   def ma

/*
import cats.Functor
import cats.Apply
import cats.Applicative

case class User(name: String)
case class Order(value: String)
case class Id(value: String)

trait Storage[F[_]] {
   def readUser(id: Id): F[User]
   def writeOrder(order: Order): F[Unit]
}

object Storage {
  def make[F[_]: Applicative]: Storage[F] = new Storage[F] {
    def readUser(id: Id): F[User] = Applicative[F].pure(User("jani"))
    def writeOrder(order: Order): F[Unit] = Applicative[F].unit
  }
}

import cats.Id
val storage = Storage.make[Option]
val storage2 = Storage.make[cats.Id]

storage.readUser(Id("a"))
storage.writeOrder(Order("a"))

storage2.readUser(Id("a"))
storage2.writeOrder(Order("a"))


import cats._
import cats.implicits._
// val a = Map("kacsa" -> 1, "macska" -> 2).toList.traverseFilter((s: String, i: Int) => if s === "kacsa" then Option((s,i)) else None)
// a

val m: Map[Int, String] = Map(1 -> "one", 3 -> "three")
val l: List[Int] = List(1, 2, 3, 4)
def asString(i: Int): Eval[Option[String]] = Later(m.get(i))
val result: Eval[List[String]] = l.traverseFilter(asString)
result.value


trait Free[M[_], A]:
  def flatMap[B](f: A => Free[M, B]): Free[M, B]

object Free:
  def pure[M[_], A](a: A): Free[M, A] = ???

trait SemiGroup[A]:
  def combine(x: A, y: A): A

trait Monoid[A] extends SemiGroup[A]:
  def identity: A

case class All(value: Boolean)

// object All:
//   given SemiGroup[All] with
//     def combine(x: All, y: All): All = (x,y) match
//       case (All(x), All(y)) => All(x && y)
//
//   given Monoid[All] with
//     def identity = All(true)
//     override def combined(x: All, y: All) = ???
//

import cats.data.*

def doSomething: Reader[Int, Int] = Reader(i => i + 1)

def something: Reader[Int, String] = for
  x <- doSomething
  y <- doSomething
  s = x.show ++ y.show
yield s

something.run(2)

val a: EitherT[Option, String, Int] = EitherT(Option(2.asRight[String]))
val b: EitherT[Option, String, Int] = EitherT(Option("sajt".asLeft[Int]))
val c: EitherT[Option, String, Int] = EitherT(Option.empty[Either[String,Int]])

a.map(_ + 2).value
b.map(_ + 2).value
c.map(_ + 2).value

val f = Applicative[Option].pure(identity)
val g = Applicative[Option].pure((x: Int, y: Int) => x + y)
f.ap(Some(2))
 */

sealed trait SomeError extends Throwable
case object VeryBadError extends SomeError
case object WorseError extends SomeError

import cats.implicits.*

val a: Either[Throwable, Int] = VeryBadError.asLeft[Int]
val b: Either[Throwable, Int] = 2.asRight[SomeError]

Either.catchNonFatal(throw new RuntimeException)
Either.catchNonFatal(2)

case class User(n: String)

trait ValamiService[F[_]]:
  def findUser(id: Int): F[User]
  def addUser(u: User): F[Unit]

trait MasikService[F[_]]:
  def doSomething: F[String]
  def doSomethingElse(i: Int): F[Int]

import cats.effect.IO
import cats.effect.std.Console
import cats.Id
import cats.*
import cats.implicits.*

object ValamiService:
  def make[F[_]: Monad: Console](v: Int): ValamiService[F] =
    new ValamiService[F]:
      def addUser(u: User): F[Unit] = Console[F].println("Adding User $u")
      def findUser(id: Int): F[User] =
        Console[F].println("Finding User $u") >> Monad[F].pure(User("a"))

  def testService: ValamiService[Id] = new ValamiService[Id]:
    def addUser(u: User): Id[Unit] = ()
    def findUser(id: Int): Id[User] = User("jani")

import cats.effect.unsafe.implicits.global
// object MasikService:
//   def make(res: String): MasikService[
ValamiService.make[IO](2).findUser(2).unsafeRunSync()
ValamiService.testService.findUser(2)

import alleycats.Extract

trait Lofasz[F[_]: Extract]:
  def f: Unit
  def g: Unit

abstract class Animal

class Dog extends Animal
class Cat extends Animal

trait List[+A]

case object Nil extends List[Nothing]
case class Cons[A](x: A, xs: List[A]) extends List[A]

Nil

1 + 2

Cons(1,Cons(2,Nil)): List[Int]
Cons(1,Cons(2,Nil))
