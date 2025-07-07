sealed trait XList[A]
object XList {
  case class XNil[A]() extends XList[A]
  case class XCons[A](head: A, rest: XList[A]) extends XList[A]
}

XList.XCons(1, XList.XNil())

sealed trait AorB
case object A extends AorB
case object B extends AorB

A: AorB
B: AorB

import cats._
import cats.data._
import cats.implicits._

def eval(a: AorB): Int = a match
  case A => 1
  case B => 2

eval(B)

val a = Inject[Int, Either[Int,String]].inj(1)

Inject[Int, Either[Int,String]].prj(a)
