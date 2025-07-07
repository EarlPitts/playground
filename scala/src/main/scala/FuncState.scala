// Random numbers with side-effects
val rng = scala.util.Random

val d = rng.nextDouble
val i = rng.nextInt

def rollDie: Int =
  val rng = scala.util.Random
  rng.nextInt(6)

// Pure RNG
trait RNG:
  def nextInt(): (Int, RNG)

// This is the definition of SimpleRNG
// and showing that it's an instance of RNG at once
case class SimpleRNG(seed: Long) extends RNG:
  def nextInt(): (Int, RNG) =
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)

val rng2 = SimpleRNG(42)
val (n1, rng3) = rng2.nextInt()

// Exercises
def nonNegativeInt(rng: RNG): (Int, RNG) =
  val (n, nextRng) = rng.nextInt()
  if n >= 0 && n <= Int.MaxValue && n != Int.MinValue
  then (n, nextRng)
  else nonNegativeInt(nextRng)

def double(rng: RNG): (Double, RNG) =
  val (n, nextRng) = rng.nextInt()
  if n > 1
  then (1.0 / n.toDouble, nextRng)
  else double(nextRng)

def intDouble(rng: RNG): ((Int, Double), RNG) =
  val (n1, rng2) = nonNegativeInt(rng)
  val (n2, rng3) = double(rng2)
  ((n1, n2), rng3)

def doubleInt(rng: RNG): ((Double, Int), RNG) =
  val (n1, rng2) = double(rng)
  val (n2, rng3) = nonNegativeInt(rng2)
  ((n1, n2), rng3)

def double3(rng: RNG): ((Double, Double, Double), RNG) =
  val (n1, rng2) = double(rng)
  val (n2, rng3) = double(rng2)
  val (n3, rng4) = double(rng3)
  ((n1, n2, n3), rng4)

def ints(count: Int)(rng: RNG): (List[Int], RNG) =
  if count == 0
  then (List(), rng)
  else
    val (n: Int, rng2: RNG) = rng.nextInt()
    val (is: List[Int], rng3: RNG) = ints(count - 1)(rng2)
    (n :: is, rng3)

// RNG State Action
type Rand[+A] = RNG => (A, RNG)

val int: Rand[Int] = rng => rng.nextInt()

def unit[A](a: A): Rand[A] =
  rng => (a, rng)

def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
  rng =>
    val (a, rng2) = s(rng)
    (f(a), rng2)

def nonNegativeEven: Rand[Int] =
  map(nonNegativeInt)(i => i - (i % 2))

def betterDouble(rng: RNG): (Double, RNG) =
  map(int)(n => scala.math.abs(1.0 / (n + 2)))(rng)

def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  rng =>
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)

def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
  map2(ra, rb)((_, _))

val randIntDouble: Rand[(Int, Double)] =
  both(int, double)

val randDoubleInt: Rand[(Double, Int)] =
  both(double, int)

def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
  ???

// Can't chain with map, need bind
// def nonNegativeLessThan(n: Int): Rand[Int] =
//   map(nonNegativeInt): i =>
//     val mod = i % n
//     if i + (n-1) - mod >= 0 then mod else nonNegativeLessThan(n)(???)

def nonNegativeLessThan(n: Int): Rand[Int] =
  rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if i + (n - 1) - mod >= 0
    then (mod, rng2)
    else nonNegativeLessThan(n)(rng2)

def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
  rng0 =>
    val (a, rng1) = r(rng0)
    f(a)(rng1)

def nonNegLessThan(n: Int): Rand[Int] =
  flatMap(nonNegativeInt): i =>
    val mod = i % n
    if i + (n - 1) - mod >= 0
    then unit(mod)
    else nonNegLessThan(n)

def mapFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
  flatMap(r): a =>
    unit(f(a))

def map2FlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  flatMap(ra): a =>
    flatMap(rb): b =>
      unit(f(a, b))

def rollDie2: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

def roll3: Rand[(Int, Int, Int)] =
  flatMap(rollDie2): d1 =>
    flatMap(rollDie2): d2 =>
      flatMap(rollDie2): d3 =>
        unit((d1, d2, d3))

// General State type
// type State[S, +A] = S => (A, S)

case class State[S, +A](runState: S => (A, S))

// type Rand[A] = State[RNG, A]

object State:
  def pure[S,A](a: A): State[S,A] =
    State(s => (a,s))
  
  def map[S, A, B](sa: State[S, A])(f: A => B): State[S, B] =
    State((s: S) =>
      val (v, s1) = sa.runState(s)
      (f(v), s)
    )

  def bind[S, A, B](sa: State[S, A])(f: A => State[S,B]): State[S, B] =
    State((s1: S) =>
        val (a, s2) = sa.runState(s1)
        f(a).runState(s2))

  def execState[S,A](sa: State[S,A]): S => S =
    (s: S) =>
      sa.runState(s)._2

  def evalState[S,A](sa: State[S,A]): S => A =
    (s: S) =>
      sa.runState(s)._1

  def get[S]: State[S,S] = State(s => (s,s))

  def put[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    bind(get)(s => put(f(s)))
