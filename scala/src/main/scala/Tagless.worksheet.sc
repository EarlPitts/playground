import cats.implicits.*
import cats.*
import java.util.UUID

trait Exp[T]:
  def lit(i: Int): T
  def neg(t: T): T
  def add(l: T, r: T): T

object Exp:
  def apply[T](implicit ev: Exp[T]) = ev

  val evaluator: Exp[Int] = new Exp[Int] {
    def lit(i: Int): Int = i
    def neg(t: Int): Int = -t
    def add(l: Int, r: Int): Int = l + r
  }

  val pp: Exp[String] = new Exp[String] {
    def lit(i: Int): String = s"$i"
    def neg(t: String): String = s"(-$t)"
    def add(l: String, r: String): String = s"($l + $r)"
  }

  object ExpSyntax:
    def lit[T: Exp](i: Int) = Exp[T].lit(i)
    def neg[T: Exp](t: T) = Exp[T].neg(t)
    def add[T: Exp](l: T, r: T) = Exp[T].add(l, r)

// It's easy to extend our language with new terms
trait ExpMul[T]:
  def mul(l: T, r: T): T

object ExpMul:
  def apply[T](implicit ev: ExpMul[T]) = ev

  val evaluator: ExpMul[Int] = new ExpMul[Int] {
    def mul(l: Int, r: Int): Int = l * r
  }

  object ExpMulSyntax:
    def mul[T: ExpMul](l: T, r: T) = ExpMul[T].mul(l, r)

// import ExpSyntax._
// import ExpMulSyntax._

// def expr1[T]: T = mul(neg(add(lit(2), lit(3))), lit(3))

def expr1[T](e: Exp[T]): T =
  e.neg(e.add(e.lit(2), e.lit(3)))

def expr2[T](e: Exp[T], m: ExpMul[T]): T =
  m.mul(e.neg(e.add(e.lit(2), e.lit(3))), e.lit(3))

expr1(Exp.evaluator)
expr1(Exp.pp)

expr2(Exp.evaluator, ExpMul.evaluator)
// expr1[String]

// We can use the same principles to have different interpreters for business logic
case class User(id: Int, name: String, age: Int)

trait UserRepo[F[_]]:
  def getUser(id: Int): F[Option[User]]

object UserRepo:
  def apply[F[_]](implicit ev: UserRepo[F]) = ev

  // It's usually better though to have explicit constructors, and not treat
  // something like this as a typeclass, when there are no algebraic laws
  // and a canonical instance
  implicit def make[F[_]: Monad]: UserRepo[F] =
    new UserRepo[F]:
      def getUser(id: Int): F[Option[User]] =
        val maybeUser =
          if id === 123
          then Some(User(123, "Jozsi", 45))
          else None
        maybeUser.pure

trait TestUserRepo extends UserRepo[Id]:
  def getUser(id: Int): Id[Option[User]] = ???

val notFoundUserRepo = new TestUserRepo:
  override def getUser(id: Int): Id[Option[User]] =
    None.pure

val dummyUserRepo = new TestUserRepo:
  override def getUser(id: Int): Id[Option[User]] =
    Some(User(123, "Jozsi", 45)).pure

// val repo = UserRepo.make[Id]
// val repo2 = dummyUserRepo.getUser(23)

def program[F[_]: Monad: UserRepo] = for
  mu1 <- UserRepo[F].getUser(123)
  mu2 <- UserRepo[F].getUser(123)
  result = for
    u1 <- mu1
    u2 <- mu1
  yield u1.age + u2.age
yield result

program[Id]

notFoundUserRepo.getUser(123)
dummyUserRepo.getUser(23)

object KVStore:
  case class User(id: UUID, email: String, loyaltyPoints: Int):
    def serialize: String = id.toString + "," + loyaltyPoints + "," + email

  object User:
    def parse(s: String): User =
      val parts = s.split(",")
      User(UUID.fromString(parts(0)), parts(2), parts(1).toInt)

  trait KVStore[F[_]]:
    def get(k: String): F[Option[String]]
    def put(k: String, v: String): F[Unit]

  object KVStore:
    def mkKVStore[F[_]: Monad] = new KVStore[F]:
      def get(k: String): F[Option[String]] = Monad[F].pure(None)
      def put(k: String, v: String): F[Unit] = Monad[F].pure(())

  trait UserRepository[F[_]]:
    def findUser(id: UUID): F[Option[User]]
    def updateUser(u: User): F[Unit]

  object UserRepository:
    def mkUserRepo[F[_]: Monad](kvStore: KVStore[F]) = new UserRepository[F]:
      def findUser(id: UUID): F[Option[User]] =
        kvStore.get(id.toString).map(serialized => serialized.map(User.parse))

      def updateUser(u: User): F[Unit] =
        val serialized = u.serialize
        for
          _ <- kvStore.put(u.id.toString, serialized)
          _ <- kvStore.put(u.email, serialized) // let's say we also maintain a by-email index
        yield ()
