enum FizzBuzzVal:
  case Fizz
  case Buzz
  case FizzBuzz
  case Num(n: Int)

import FizzBuzzVal.*

def FizzBuzzify(n: Int): List[FizzBuzzVal] =
  def f(n: Int): FizzBuzzVal =
    if n % 15 == 0 then FizzBuzz
    else if n % 3 == 0 then Fizz
    else if n % 5 == 0 then Buzz
    else Num(n)
  List.range(1,n).map(f)
