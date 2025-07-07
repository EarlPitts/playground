import cats.*
import cats.implicits.*
import cats.data.{Reader, ReaderT}

case class Services(adder: (Int, Int) => Int, subtracter: (Int, Int) => Int)

def doSomething(x: Int, y: Int): Reader[Services, Int] =
  Reader(s => s.adder(x, y))

def doSomethingElse(x: Int, y: Int): Reader[Services, Int] =
  Reader(s => s.subtracter(x, y))

def program(x: Int, y: Int): Reader[Services, Int] = for
  a <- doSomething(x, y)
  result <- doSomethingElse(a, y)
yield result

val s = Services((a: Int, b: Int) => a + b, (a: Int, b: Int) => a - b)
program(1, 2).run(s)

val f: Int => String = _.toString
val g: String => List[Char] = _.toList

(g compose f)(123)
Reader(f).run(2)
// Reader(f) >>= (if (_ === "12") then Reader.pure((x: String) => 21) else Reader(g))

val h = for
  a <- Reader(f)
  res <- Reader(g) //.whenA(a === "12")
yield res

object WithCurrying:
  case class DBConnection():
    def get(id: Int) = Item(id, "goat")
    def rename(id: Int, newName: String) = Item(id, newName)

  case class Item(id: Int, name: String)

  def getItem(id: Int)(db: DBConnection): Item =
    db.get(id)

  def renameItem(item: Item, newName: String)(db: DBConnection): Item =
    db.rename(item.id, newName)

  def renameItemWithId(id: Int, newName: String)(db: DBConnection): Item =
    val item = getItem(id)(db)
    renameItem(item, newName)(db)

import WithCurrying.*
val item = renameItemWithId(123, "dog")(DBConnection())


object WithReader:
  case class DBConnection():
    def get(id: Int) = Item(id, "goat")
    def rename(id: Int, newName: String) = Item(id, newName)

  case class Item(id: Int, name: String)

  def getItem(id: Int) = Reader[DBConnection, Item] { db =>
    db.get(id)
  }

  def renameItem(item: Item, newName: String) = Reader[DBConnection, Item] {
    db => db.rename(item.id, newName)
  }

  def renameItemWithId(id: Int, newName: String): Reader[DBConnection, Item] =
    for
      item <- getItem(id)
      renamed <- renameItem(item, newName)
    yield renamed

val item2 = WithReader.renameItemWithId(123, "dog").run(WithReader.DBConnection())

object WithReaderT:
  case class DBConnection():
    def get(id: Int) = if id === 123 then Some(Item(id, "goat")) else None
    def rename(id: Int, newName: String) = Item(id, newName)

  case class Item(id: Int, name: String)

  def getItem(id: Int) = ReaderT[Option, DBConnection, Item] { db =>
    db.get(id)
  }

  def renameItem(item: Item, newName: String) = Reader[DBConnection, Item] {
    db => db.rename(item.id, newName)
  }

  def renameItemWithId(id: Int, newName: String): ReaderT[Option, DBConnection, Item] =
    for
      item <- getItem(id)
      renamed <- renameItem(item, newName).lift[Option]
    yield renamed

val item3 = WithReaderT.renameItemWithId(13, "dog").run(WithReaderT.DBConnection())

def add(x: Int, amount: Int): Int = x + amount

// def userFinder(id: Int):

def raiseSalary(salary: Int, amount: Int)(adder: (Int, Int) => Int): Int =
  adder(salary, amount)

raiseSalary(1000, 200)(add)

type MyId[A] = A

object MyId:
  def apply[A](a: A): MyId[A] = a

MyId(12)

import cats.data.Kleisli

Kleisli[Option, Int, Int] { (i: Int) =>
  Some(i + 1)
}.run(2)

