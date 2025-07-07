import cats.*
import cats.implicits.*
import cats.data.*
import cats.effect.*

case class Config(url: String, token: String)

def saySomething(times: Int): ReaderT[IO, Config, List[String]] =
  ReaderT { c =>
    val say = s"Saying the ${c.url}"
    List.fill(times)(say).traverse(IO.println) >>
      List.fill(times)(say).pure
  }

def totallySendTheRequest(why: String): ReaderT[IO, Config, String] =
  ReaderT { c =>
    IO.pure(s"Sending the request to ${c.url}, because $why")
      .flatTap(IO.println)
  }

import cats.effect.std.Console

trait Logger[F[_]]:
  def log: String => F[Unit]

object Logger:
  def mkLogger[F[_]: Monad: Console] = new Logger[F]:
    def log: String => F[Unit] = s => Console[F].println(s)

def logger[M[_]: Console](log: String): M[Unit] = Console[M].println(log)

type ConfigIOReader[A] = ReaderT[IO, Config, A]


def main[M[_]: Monad](
    somethingSaying: Int => M[List[String]],
    requestSending: String => M[String],
    logger: Logger[M]
): M[List[String]] = for
  says <- somethingSaying(3)
  _ <- logger.log("said")
  sends <- requestSending("why not")
  _ <- logger.log("sent")
yield sends :: says

val c = Config("example.com", "12345")

import cats.effect.unsafe.implicits.global

main[ConfigIOReader](saySomething, totallySendTheRequest, Logger.mkLogger).run(c).unsafeRunSync()

type W[X] = Writer[Map[String, Int], X]

def mockSaying(times: Int): W[List[String]] = Writer(Map("sayer" -> 1), List.fill(times)("valami"))
def mockSending(why: String): W[String] = Writer(Map("sender" -> 1), why)
def mockLogger(log: String): W[Unit] = Map("log" -> 1).tell

val testLogger = new Logger[IO]:
  def log = _ => IO.unit

// main(mockSaying, mockSending, testLogger)


type Key = Char
type Value = Int

case class KVStoreState(funCalls: Map[String, Int], store: Map[Key, Value])

type MockState[A] = State[KVStoreState, A]

trait KeyValueStore[F[_]]:
  def get(key: Key): F[Option[Value]]
  def put(key: Key, value: Value): F[Unit]
  def delete(key: Key): F[Unit]

object KeyValueStore: 
  def stub(m: Map[Key, Value]): KeyValueStore[Id] = new KeyValueStore[Id]:
    def delete(key: Key): Id[Unit] = m - key
    def get(key: Key): Id[Option[Value]] = m.get(key)
    def put(key: Key, value: Value): Id[Unit] = m + (key -> value)

  def mock: KeyValueStore[MockState] = new KeyValueStore[MockState]:
    def incrementCall(funName: String, funCalls: Map[String, Int]): Map[String, Int] =
      funCalls.updatedWith(funName)(count => Some(count.getOrElse(0) + 1))

    def delete(key: Key): MockState[Unit] = State { s =>
      val newFunCalls = incrementCall("delete", s.funCalls)
      val newStore = s.store - key
      (KVStoreState(newFunCalls, newStore), ())
    }
    def get(key: Key): MockState[Option[Value]] = for {
      s <- State.get[KVStoreState]
      newFunCalls = incrementCall("get", s.funCalls)
      _ <- State.set(s.copy(funCalls = newFunCalls))
    } yield s.store.get(key)
    def put(key: Key, value: Value): MockState[Unit] = for{
      s <- State.get[KVStoreState]
      newFunCalls = incrementCall("put", s.funCalls)
      newStore = s.store + (key -> value)
      _ <- State.set(KVStoreState(newFunCalls, newStore))
    } yield ()

val m = Map.from(List.range('a', 'z').zip(List.iterate(1, 26)(_ + 1)))
val stub = KeyValueStore.stub(m)

val initialState = KVStoreState(Map.empty[String, Int], m)

val mock = KeyValueStore.mock

val program = for {
  get1 <- mock.get('a')
  _ <- mock.delete('a')
  get2 <- mock.get('a')
  _ <- mock.put('a', 74)
  get3 <- mock.get('a')
} yield (get1, get2, get3)

program.run(initialState).value
