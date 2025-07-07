// package Monoid

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

val stringMonoid: Monoid[String] = new:
  def combine(a1: String, a2: String) = a1 + a2
  val empty = ""
  
def listMonoid[A]: Monoid[List[A]] = new:
  def combine(a1: List[A], a2: List[A]) = a1 ++ a2
  val empty = Nil

val intAddition: Monoid[Int] = new:
  def combine(i1: Int, i2: Int) = i1 + i2
  val empty = 0

val intMultiplication: Monoid[Int] = new:
  def combine(i1: Int, i2: Int) = i1 * i2
  val empty = 1

val booleanOr: Monoid[Boolean] = new:
  def combine(b1: Boolean, b2: Boolean) = b1 || b2
  val empty = true

val booleanAnd: Monoid[Boolean] = new:
  def combine(b1: Boolean, b2: Boolean) = b1 && b2
  val empty = false

def optionMonoid[A]: Monoid[Option[A]] = new:
  def combine(a1: Option[A], a2: Option[A]) =
    (a1,a2) match
      case (Some(a),_) => a1
      case (_,Some(a)) => a2
      case _           => None
  def empty = None

def endoMonoid[A]: Monoid[A => A] = new:
  def combine(f1: A => A, f2: A => A) = f1 compose f2
  val empty = x => x

val nums = List(1,2,3,4)

val s = nums.foldRight(intAddition.empty)(intAddition.combine)

def combineAll[A](as: List[A], m: Monoid[A]): A =
  as.foldRight(m.empty)(m.combine)

def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
  as.map(f).foldRight(m.empty)(m.combine)

val combinedLengths = foldMap(List("one", "two", "three"), intAddition)(l => l.size)
