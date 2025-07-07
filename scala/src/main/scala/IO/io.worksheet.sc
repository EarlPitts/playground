import cats.*
import cats.implicits.*

object IOMonoid:
  trait IO:
    self => // This is a self-type, aliasing "this"
    def unsafeRun: Unit
    def ++(io: IO): IO = new:
      def unsafeRun =
        self.unsafeRun
        io.unsafeRun

  object IO:
    def empty: IO = new:
      def unsafeRun = ()

    implicit val monoid: Monoid[IO] = new Monoid[IO]:
      def combine(x: IO, y: IO): IO = x ++ y
      def empty: IO = IO.empty


  def PrintLine(d: => String): IO = new:
    def unsafeRun: Unit = println(d)

  // This won't work, we have no way to "yield" the value
  def ReadLine: IO = new:
    def unsafeRun: Unit = scala.io.StdIn.readLine()

  // class PrintLine(d: => String) extends IO:
  //   override def unsafeRun: Unit = println(d)


import IOMonoid.*

case class Player(name: String, score: Int)

def contest(p1: Player, p2: Player): IO =
  PrintLine(winnerMsg(winner(p1, p2)))

def winner(p1: Player, p2: Player): Option[Player] =
  if p1.score > p2.score then Some(p1)
  else if p2.score > p1.score then Some(p2)
  else None

def winnerMsg(p: Option[Player]): String =
  p.map { case Player(_, name) => s"${name} is the winner!" }
    .getOrElse("It's a draw.")

val p1 = Player("jani", 14)
val p2 = Player("bela", 13)

val p3 = Player("imre", 14)
val p4 = Player("laci", 13)

(contest(p1, p2) ++ contest(p3, p4)).unsafeRun

val prints = (1 to 5).toList.map((x: Int) => PrintLine(x.show))

Monoid[IO].combineAll(prints).unsafeRun
prints.fold(Monoid[IO].empty)(Monoid[IO].combine).unsafeRun

// Won't work
def converter: IO =
  val prompt: IO = PrintLine("Enter a temperature: ")
  val input: IO = ReadLine
  prompt ++ input
