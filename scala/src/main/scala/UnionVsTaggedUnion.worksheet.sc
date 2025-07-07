import Tagged.*

enum Tagged:
  case A(a: Int)
  case B(b: String)

val x: Int | String = "sajt"
val y: Tagged = B("sajt")

x
y

// We match on the type at runtime
x match {
  case _: Int    => "I'm an Int"
  case _: String => "I'm a String"
}

// We can match on the constructor (which acts like a "tag")
y match {
  case A(a) => "I'm an A"
  case B(b) => "I'm a B"
}
