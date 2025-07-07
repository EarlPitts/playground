import cats.*
import cats.implicits.*

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

p.runPred("longerthantencharacters")

val ordering = Order[Int]
  .contramap[String](_.length)

List("kutya", "cica", "malac", "kecskebeka").sortWith(ordering.gt)
