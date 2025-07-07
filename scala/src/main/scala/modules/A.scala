package modules

enum Value:
  private case BoolValue(value: Boolean)
  private case IntValue(value: Int)
  private case FloatValue(value: Float)
  private case SpringValue(value: String)

// Scala 2
// trait Value
// private case class BoolValue(value: Boolean) extends Value
// private case class IntValue(value: Int) extends Value
// private case class FloatValue(value: Float) extends Value
// private case class SpringValue(value: String) extends Value

object Value:
  def mkInt(value: Int): Option[Value] =
    if value < 0 || value > 100 then None else Some(IntValue(value))

val x: Option[Value] = Value.mkInt(12)

// Internals cannot be used directly!
// val y: Value = Value.BoolValue(true)
//
// def f(v: Value) = v match {
//   case Value.BoolValue(value) => ???
// }
