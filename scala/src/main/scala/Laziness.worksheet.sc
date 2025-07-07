def and = (x: Boolean) =>
  (y: Boolean) =>
    if x then { println("woohoo!\nWOOOOT"); y }
    else false

and(false)(true)

// Delay by wrappin inside lambda
def if2[A](
    guard: Boolean,
    onTrue: () => A,
    onFalse: () => A
): A = guard match
  case true  => onTrue()
  case false => onFalse()

// Syntax sugar
def if3[A](guard: Boolean, onTrue: => A, onFalse: => A): A =
  guard match
    case true  => onTrue
    case false => onFalse

def f: Int = f
if2(true, () => 12, () => ???)
if3(true, 12, f)

lazy val k = 4

def g(x: => Int): Int = 3

// The block won't be evaluated, as it's not used at all
g({ println("hey"); 2 })
