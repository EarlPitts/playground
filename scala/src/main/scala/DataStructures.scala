object DataStructures:

  enum List[+A]:
    case Nil
    case Cons(head: A, tail: List[A])

  object List:
    // Apply simply applies List as a function
    // serving as the constructor
    def apply[A](as: A*): List[A] =
      if as.isEmpty then Nil
      else Cons(as.head, apply(as.tail*))

    // def toString[A](l: List[A]): String =
    //   "[" ++ foldr((x,s) => x.toString ++ "," ++ s, "]", l)

    def foldr[A,B](f: (A,B) => B, z: B, l: List[A]): B =
      l match
        case Nil => z
        case Cons(x,xs) => f(x,foldr(f,z,xs))

    def foldl[A,B](f: (B,A) => B, z: B, l: List[A]): B =
      def go(acc: B, l: List[A]): B =
        l match
          case Nil => acc
          case Cons(x,xs) => go(f(acc,x),xs)
      go(z,l)

    def foldr2[A,B](f: (A,B) => B, z: B, l: List[A]): B =
      // TODO Figure out how to import this flip from Intro
      def flip[C](f: (A,B) => C): (B,A) => C =
        (b,a) => f(a,b)
      foldl(flip(f),z,l)

    def tail[A](l: List[A]): List[A] =
      l match
        case Nil => Nil
        case Cons(_, xs) => xs

    def append[A](l1: List[A], l2: List[A]): List[A] =
      l1 match
        case Nil => l2
        case Cons(x,xs) => Cons(x,append(xs,l2))

    def map[A,B](f: A => B, as: List[A]): List[B] =
      as match
        case Nil => Nil
        case Cons(a,as) => Cons(f(a),map(f,as))

    def filter[A](p: A => Boolean, as: List[A]): List[A] =
      as match
        case Nil => Nil
        case Cons(a,as) =>
          if p(a) then Cons(a,filter(p, as)) else filter(p,as)

    def flatMap[A,B](f: A => List[B], as: List[A]): List[B] =
      flatten(map(f,as))

    def filter2[A](p: A => Boolean, as: List[A]): List[A] =
      flatMap((a: A) => if p(a) then List(a) else Nil, as)

    def zipWith[A,B,C](f: (A,B) => C, as: List[A], bs: List[B]): List[C] =
      (as,bs) match
        case (Cons(a,as), Cons(b,bs)) => Cons(f(a,b),zipWith(f,as,bs))
        case _                        => Nil

    def zip[A,B](as: List[A], bs: List[B]): List[(A,B)] =
      zipWith((a: A, b: B) => (a,b), as, bs)

    // With foldr
    // TODO Can Cons be used without wrapping it in a lambda?
    // Cons(_,_) didn't work
    def append2[A](l1: List[A], l2: List[A]): List[A] =
      foldr((x: A,xs: List[A]) => Cons(x,xs), l2, l1)

    def flatten[A](l: List[List[A]]): List[A] =
      foldr((l: List[A], ls: List[A]) => append(l,ls),Nil,l)

    def range(a: Int, b: Int): List[Int] =
      if a > b then Nil else Cons(a,range(a+1,b))

    def init[A](l: List[A]): List[A] =
      l match
        case Cons(x,Nil) => Nil
        case Cons(x,xs)  => Cons(x,init(xs))
        case _           => sys.error("Oh no")

    def length[A](l: List[A]): Int =
      foldr((_,acc: Int) => acc + 1, 0, l)

    def reverse[A](l: List[A]): List[A] =
      def go(l: List[A], acc: List[A]): List[A] =
        l match
          case Nil        => acc
          case Cons(x,xs) => go(xs,Cons(x,acc))
      go(l,Nil)

    def reverseWithFold[A](l: List[A]): List[A] =
      foldl((acc: List[A],x: A) => Cons(x,acc),Nil,l)

    def take[A](a: Int, b: List[A]): List[A] =
      b match
        case Nil => Nil
        case Cons(b,bs) => if a == 0
                          then Nil
                          else Cons(b,take(a-1,bs))

    def drop[A](a: Int, b: List[A]): List[A] =
      (a,b) match
        case (0,b) => b
        case (a,Nil) => Nil
        case (a,Cons(b,bs)) => drop(a-1,bs)

    def takeWhile[A](p: A => Boolean, l: List[A]): List[A] =
      l match
        case Nil => Nil
        case Cons(x,xs) => if p(x)
                          then Cons(x,takeWhile(p,xs))
                          else Nil

    def dropWhile[A](p: A => Boolean, l: List[A]): List[A] =
      l match
        case Nil => Nil
        case Cons(x,xs) => if p(x)
                          then dropWhile(p,xs)
                          else xs

    def replicate[A](n: Int, a: A): List[A] =
      n match
        case 0 => Nil
        case n => Cons(a,replicate(n-1,a))

    def safeHead[A](l: List[A]): Option[A] =
      l match
        case Nil => None
        case Cons(x, _) => Some(x)

    def all[A](p: A => Boolean, l: List[A]): Boolean =
      foldr((x: A, acc: Boolean) => p(x) && acc, true, l)

    def any[A](p: A => Boolean, l: List[A]): Boolean =
      foldr((x: A, acc: Boolean) => p(x) || acc, false, l)

    // TODO This is wrong, sub can be longer, so the zip won't work
    def isSubsequence[A](sup: List[A], sub: List[A]) =
      def f(l: List[A]): List[List[A]] = l match
        case Nil => Nil
        case Cons(a,as) => Cons(Cons(a,as),f(as))
      def p(x: (A,A)): Boolean = x match { case (x,y) => (x == y) }
      val subs = map(((as: List[A]) => zip(as,sub)), f(sup))
      any(_ == true, map((x: List[(A,A)]) => all(p,x), subs))

  enum Tree[+A]:
    case Leaf
    case Node(l: Tree[A], x: A, r: Tree[A])

    // You could also define methods here, and refer to the value as "this",
    // so you don't have to explicitly pass the value as an argument

  object Tree:
    def apply(xs: Int*): Tree[Int] =
      fromList(List(xs*))

    def size[A](t: Tree[A]): Int =
      t match
        case Leaf        => 0
        case Node(l,_,r) => 1 + size(l) + size(r)

    def depth[A](t: Tree[A]): Int =
      def max(x: Int, y: Int) = if x > y then x else y
      t match
        case Leaf                           => 0
        case Node(l: Tree[A],_ ,r: Tree[A]) => 1 + max(depth(l),depth(r))

    def insertTree(t: Tree[Int], x: Int): Tree[Int] =
      t match
        case Leaf => Node(Leaf, x, Leaf)
        case Node(l,y,r) => if x > y
                            then Node(l,y,insertTree(r,x))
                            else Node(insertTree(l,x),y,r)

  //                  A             B
    def fromList(l: List[Int]): Tree[Int] =
      List.foldl(insertTree,Leaf,l)

  // object Queue:
  //   def enqueue[A](q: Queue[A], x: A): Queue[A]

  // You can import anywhere
  import List.*

  trait Functor[F[_]]:
    def fmap[A,B](f: A => B, fa: F[A]): F[B]

  given Functor[List] with
    def fmap[A, B](f: A => B, fa: List[A]): List[B] =
        fa match
          case Nil         => Nil
          case Cons(a, as) => Cons(f(a),fmap(f,as))

  val l = range(0,10)
  val asd = List(1 to 10)

  // Short-circuiting with foldr
  val x2 = foldr((x:Int ,acc: Int) => if x == 0 then 0 else x * acc, 1, l)

  // enum Nat:
  //   case Z
  //   case S(n: Nat)
  //
  // object Nat:
  //   def apply = ???

  val x = List(1,2,3)

object Pizza:
  enum Crust:
    case Thin, Thick

  enum Size:
    case Small, Medium, Large

  case class Pizza(c: Crust, s: Size):
    override def toString: String = s"Delicious pizza with $c crust and $s size"
