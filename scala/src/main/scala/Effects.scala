import cats.Monad


case class Player(name: String, score: Int)

def contest(p1: Player, p2: Player): Unit =
  if p1.score > p2.score then
    println(s"${p1.name} is the winner!") 
  else if p2.score > p1.score then
    println(s"${p2.name} is the winner!") 
  else 
    println("It's a draw")

def winner(p1: Player, p2: Player): Option[Player] =
  if p1.score > p2.score then
    Some(p1)
  else if p2.score > p1.score then
    Some(p2)
  else 
    None

def contest2(p1: Player, p2: Player): Unit =
  winner(p1,p2) match
    case Some(p) => println(s"${p.name} is the winner!") 
    case None    => println("It's a draw")

def winnerMsg(p: Option[Player]): String = p
  .map:
    case Player(name, _) => s"$name is the winner!"
  .getOrElse("It's a draw")

def contest3(p1: Player, p2: Player): Unit =
  println(winnerMsg(winner(p1,p2)))

val p1 = Player("Bela", 3)
val p2 = Player("Jani", 2)

// trait IO:
//   self =>
//   def unsafeRun: Unit
//   def ++(io: IO): IO = new:
//     def unsafeRun =
//       self.unsafeRun
//       io.unsafeRun

// object IO:
//   def empty: IO = new:
//     def unsafeRun = ()

def PrintLine(msg: String): IO[Unit] = new:
  def unsafeRun = println(msg)

def contest4(p1: Player, p2: Player): IO[Unit] =
  PrintLine(winnerMsg(winner(p1,p2)))

trait IO[A]:
  self =>
  def unsafeRun: A
  def map[B](f: A => B): IO[B] = new:
    def unsafeRun = f(self.unsafeRun)
  def flatMap[B](f: A => IO[B]) = new:
    def unsafeRun = f(self.unsafeRun).unsafeRun

object IO:
  def apply[A](a: => A): IO[A] = new:
    def unsafeRun = a

  // given monad: Monad[IO] with
  //   def unit[A](a: => A): IO[A] = IO(a)
  //   extension [A](fa: IO[A])
  //     override def flatMap[B](f: A => IO[B]) =
  //       fa.flatMap(f)
