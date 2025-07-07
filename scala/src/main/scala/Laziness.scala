def and = (x: Boolean) => (y: Boolean) => if x then {println("woohoo!\nWOOOOT"); y} else false

val x = and(false)(true)

def if2[A](guard: Boolean, onTrue: () => A, onFalse: () => A): A = guard match
  case true => onTrue()
  case false => onFalse()

// Syntax sugar
def if3[A](guard: Boolean, onTrue: => A, onFalse: => A): A = guard match
  case true => onTrue
  case false => onFalse

def f: Unit = f
val y = if2(true, () => 12, () => 23)
val z = if3(true, 12, f)
