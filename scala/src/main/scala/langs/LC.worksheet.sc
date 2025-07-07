import cats.*
import cats.implicits.*

trait Exp[T] {
  def lit(i: Int): T
  def neg(t: T): T
  def add(l: T, r: T): T
}

object Exp:
  val printer: Exp[String] = new Exp[String] {
    def add(l: String, r: String): String = s"($l + $r)"
    def lit(i: Int): String = i.show
    def neg(t: String): String = s"-($t)"
  }

  val evaluator: Exp[Int] = new Exp[Int] {
    def add(l: Int, r: Int): Int = l + r
    def lit(i: Int): Int = i
    def neg(t: Int): Int = -t
  }

trait Mult[T] {
  def mult(l: T, r: T): T
}

object Mult:
  val printer: Mult[String] = new Mult[String] {
    def mult(l: String, r: String): String = s"($l * $r)"
  }

  val evaluator: Mult[Int] = new Mult[Int] {
    def mult(l: Int, r: Int): Int = l * r
  }
  
sealed trait Tree
final case class Leaf(s: String) extends Tree
final case class Node(s: String, ts: List[Tree]) extends Tree

def toTree: Exp[Tree] with Mult[Tree] = new Exp[Tree] with Mult[Tree] {
  def lit(i: Int): Tree = Node("Lit", List(Leaf(i.toString)))
  def neg(t: Tree): Tree = Node("Neg", List(t))
  def add(l: Tree, r: Tree): Tree = Node("Add", List(l , r))
  def mult(l: Tree, r: Tree): Tree = Node("Mult", List(l , r))
}

def program[T](i: Exp[T], m: Mult[T]): T =
  m.mult(i.add(i.lit(8), i.neg(i.add(i.lit(1), i.lit(2)))), i.lit(3))

program[Tree](toTree, toTree)

program(Exp.printer, Mult.printer)
program(Exp.evaluator, Mult.evaluator)
