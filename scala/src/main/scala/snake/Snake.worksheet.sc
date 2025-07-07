import cats.effect.IO
import cats.*
import cats.implicits.*
import Direction.*

case class GameState(snake: SnakeState, food: FoodState)
case class SnakeState(parts: List[Coordinates], dir: Direction)
case class FoodState(pos: Coordinates, counter: Int)

// case class Input()

case class Coordinates(x: Int, y: Int)

enum Direction:
  case Up, Down, Left, Right

// sealed trait Snake[F[_]]:
//   def move(d: Direction)

// sealed trait Food[F[_]]:
//   def move(d: Direction)

def move(s: SnakeState, d: Option[Direction]): SnakeState =
  val dir = d.getOrElse(s.dir)
  val newCoord = adjacent(s.parts.head, dir)
  SnakeState(newCoord :: s.parts.dropRight(1), dir)

def grow(s: SnakeState, d: Option[Direction]): SnakeState = // TODO generalize this
  val dir = d.getOrElse(s.dir)
  val newCoord = adjacent(s.parts.head, dir)
  SnakeState(newCoord :: s.parts, dir)

def adjacent(c: Coordinates, d: Direction): Coordinates = d match
  case Down  => Coordinates(c.x, c.y - 1)
  case Up    => Coordinates(c.x, c.y + 1)
  case Left  => Coordinates(c.x - 1, c.y)
  case Right => Coordinates(c.x + 1, c.y)

def checkEaten(s: SnakeState, f: FoodState, d: Option[Direction]): Boolean =
  val dir = d.getOrElse(s.dir)
  adjacent(s.parts.head, dir) == f.pos

def updateFood(f: FoodState, pos: Coordinates): FoodState =
  if f.counter == 0
  then FoodState(pos, 10)
  else f.copy(counter = f.counter - 1)

def step(s: GameState, pos: Coordinates, d: Option[Direction]): GameState =
  if checkEaten(s.snake, s.food, d)
  then
    val nextFood = s.food.copy(counter = 0) // Force update next turn
    val nextSnake = grow(s.snake, d)
    GameState(nextSnake, nextFood)
  else
    val nextFood = updateFood(s.food, pos)
    val nextSnake = move(s.snake, d)
    GameState(nextSnake, nextFood)


def display(s: GameState): IO[Unit] =
  ???

val parts = List(
  Coordinates(0, 4),
  Coordinates(0, 3),
  Coordinates(0, 2),
  Coordinates(0, 1),
  Coordinates(0, 0)
)
val s = SnakeState(parts, Up)

move(s, Some(Right))
