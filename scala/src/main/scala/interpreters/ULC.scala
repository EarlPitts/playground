package ULC

import collection.immutable.List.*
import parsley.Success
import parsley.Parsley
import parsley.character.{char, lower, spaces}
import parsley.combinator.{some, eof}
import parsley.implicits.character.{charLift, stringLift}

import ULC.Term.*
import ULC.RawTerm.*

lazy val p: Parsley[RawTerm] = pVar <|> pAbs <|> pApp

lazy val pApp: Parsley[RawTmApp] = for {
  _  <- char('(')
  t1 <- p
  _  <- spaces
  t2 <- p
  _  <- char(')')
} yield RawTmApp(t1, t2)

lazy val pVar: Parsley[RawTmVar] = some(lower).map(x => RawTmVar(x.toString()))

lazy val pAbs: Parsley[RawTmAbs] = for {
  _ <- char('\\')
  v <- some(lower).map(_.toString())
  _ <- char('.')
  t <- p
} yield RawTmAbs(v,t)

enum RawTerm:
  case RawTmVar(n: Name)
  case RawTmApp(t1: RawTerm, t2: RawTerm)
  case RawTmAbs(v: Name, t: RawTerm)

enum Term:
  case TmVar(ind: Int, size: Int)
  case TmApp(t1: Term, t2: Term)
  case TmAbs(t1: Term)

type Name = String
type Context = List[Name]
  
object Term:

  // TODO Standard lib

  def toNameless(t: RawTerm): Term =
    def go(t: RawTerm, ctx: Context): Term =
      t match
        case RawTerm.RawTmApp(t1, t2) => TmApp(go(t1,ctx), go(t2,ctx))
        case RawTerm.RawTmAbs(n,t)    => TmAbs(go(t,n :: ctx))
        case RawTerm.RawTmVar(n)      => TmVar(ctx.indexOf(n),ctx.length)
    go(t,List())
  
  def isVal(t: Term): Boolean =
    t match
      case TmAbs(_) => true
      case _        => false
  
  def shift(d: Int, t: Term): Term =
    def walk(c: Int, t: Term): Term =
      t match
        case TmVar(x,s)   => if x >= c then TmVar(x+d,s+d) else TmVar(x,s+d)
        case TmAbs(t)     => TmAbs(walk(c+1,t))
        case TmApp(t1,t2) => TmApp(walk(c,t1),walk(c,t2))
    walk(0,t)

  def subst(i: Int, v: Term, t: Term): Term =
    def walk(c: Int, t: Term): Term =
      t match
        case TmVar(x,n)   => if x == i + x then shift(c,v) else TmVar(x,n)
        case TmAbs(t)     => TmAbs(walk(c+1,t))
        case TmApp(t1,t2) => TmApp(walk(c,t1),walk(c,t2))
    walk(0,t)

  def substTop(s: Term, t: Term): Term =
    shift(-1, (subst(0, (shift(1,s)), t)))

  // Small-step semantics
  def reduce(t: Term): Term =
    t match
      case TmApp(t@(TmAbs(b)),t2) => if isVal(t2) then substTop(t2,b) else TmApp(t,reduce(t2))
      case TmApp(t1,t2)           => TmApp(reduce(t1),t2)

  def eval(t: Term): Term =
    if isVal(t) then t else eval(reduce(t))

def eval(s: String): Option[Term] =
  p.parse(s) match
    case Success(t) => Some(Term.eval(Term.toNameless(t)))
    case _                   => None
