package Monoids

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit instance: Monoid[A]) = instance
}

object MonoidSyntax {
  implicit class MonoidOps[A](x: A) {
    def combine(y: A)(implicit m: Monoid[A]): A = m.combine(x, y)
  }
}

object MonoidInstances {
  implicit val all: Monoid[Boolean] = {
    new Monoid[Boolean] {
      def combine(x: Boolean, y: Boolean): Boolean = x && y
      def empty: Boolean = true
    }
  }

  implicit val any: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(x: Boolean, y: Boolean): Boolean = x || y
      def empty: Boolean = false
    }

  import SuperAdder.Order
  implicit val orderMonoid: Monoid[Order] =
    new Monoid[Order] {
      def empty: Order = Order(0, 0)
      def combine(x: Order, y: Order): Order = (x, y) match
        case (Order(t, c), Order(t1, c1)) => Order(t + t1, c + c1)
    }
}

object SuperAdder {
  import cats.Monoid
  import cats.implicits._

  def add(items: List[Int]): Int =
    cats.Monoid[Int].combineAll(items)

  def add2(items: List[Int]): Int =
    items.foldRight(cats.Monoid[Int].empty)(cats.Monoid[Int].combine)

  // def add3[A](items: List[A]): A =
  //   items.foldRight(cats.Monoid[A].empty)(cats.Monoid[A].combine)

  def add4[A](items: List[A])(implicit monoid: cats.Monoid[A]): A =
    items.foldRight(monoid.empty)(monoid.combine)

  case class Order(totlCost: Double, quantity: Double)
}

object Invaritan {

  import cats._
  import cats.syntax.invariant._
  import cats.instances._

  // Symbols are like atoms in other languages
  implicit val symbolMonoid: cats.Monoid[Symbol] =
    cats.Monoid[String].imap(Symbol.apply)(_.name)

}

@main
def main: Unit =
  import MonoidSyntax._
  import MonoidInstances.any

  println(false.combine(true))
