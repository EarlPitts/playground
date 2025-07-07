object Algebras:

  // Semigroup
  trait Semigroup[A]:
    def combine(a1: A, a2: A): A

  object Semigroup:
    def stringSemigroup: Semigroup[String] = new:
      def combine(a1: String, a2: String) = a1 + a2

    def listSemigroup[A]: Semigroup[List[A]] = new:
      def combine(a1: List[A], a2: List[A]) = a1 ++ a2

  // Monoid
  trait Monoid[A]:
    def combine(a1: A, a2: A): A
    def empty: A

  object Monoid:
    def stringMonoid: Monoid[String] = new:
      def combine(a1: String, a2: String) = a1 + a2
      def empty = ""

    def listMonoid[A]: Monoid[List[A]] = new:
      def combine(a1: List[A], a2: List[A]) = a1 ++ a2
      def empty = Nil

    def intAddition: Monoid[Int] = new:
      def combine(a1: Int, a2: Int) = a1 + a2
      def empty = 0

    def intMultiplication: Monoid[Int] = new:
      def combine(a1: Int, a2: Int) = a1 * a2
      def empty = 1

    def booleanOr: Monoid[Boolean] = new:
      def combine(b1: Boolean, b2: Boolean) = b1 || b2
      def empty = false
      
    def booleanAnd: Monoid[Boolean] = new:
      def combine(b1: Boolean, b2: Boolean) = b1 && b2
      def empty = true

    def optionMonoid[A]: Monoid[Option[A]] = new:
      def combine(a1: Option[A], a2: Option[A]) =
        (a1, a2) match
          case (Some(a1), _) => Some(a1)
          case (_, Some(a2)) => Some(a2)
          case _             => None
      def empty = None

    def endoMonoid[A]: Monoid[A => A] = new:
      def combine(f: A => A, g: A => A) = f compose g
      def empty = identity

  import Semigroup.*
  import Monoid.*

  val _ = endoMonoid[Int].combine(_ + 1, _ + 2)(2) // 5

  // Think of m as a typeclass constraint
  // Or as a bunch of functions you can use on A
  def combineAll[A](as: List[A], m: Monoid[A]): A =
    as.foldRight(m.empty)(m.combine)

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldRight(m.empty)(m.combine)

  val _ = foldMap(List("a","ab","abc"), intAddition)(_.length)

  // TODO
  def foldr[A,B](f: A => B => B, z: B, as: List[A]): B =
    def m: Monoid[B] = new:
      def combine(b1: B, b2: B): B = ???
      def empty = z
    foldMap(as, m)(x => f(x)(z))

  // Typeclasses

  // Here the m monoid instance is not passed explicitly
  // instead it's something called a *context parameter*
  // Scala will try to find an instance defined by the
  // *given* keyword
  def foldMap2[A,B](as: List[A])(f: A => B)(using m: Monoid[B]): B =
    as.map(f).foldRight(m.empty)(m.combine)

  // You can think of this as the
  // default Int monoid that will be used
  given Monoid[Int] = new:
    def combine(a1: Int, a2: Int) = a1 + a2
    def empty = 0

  val _ = foldMap2(List("a","ab","abc"))(_.length)

  // To force a particular monoid, the *using*
  // keyword can be used
  val _ = foldMap2(List("a","ab","abc"))(_.length)(using intMultiplication)
