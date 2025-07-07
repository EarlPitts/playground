import cats.Applicative

enum Term:
  case TmTrue
  case TmFalse
  case TmIf(t1:Term, t2:Term, t3:Term)
  case TmZero
  case TmPred(t1:Term)
  case TmSucc(t1:Term)
  case TmIsZero(t1:Term)

object Term:
  def isNumeric(t:Term): Boolean =
    t match
      case Term.TmZero    => true
      case Term.TmSucc(t) => isNumeric(t)
      case _              => false

  // Smallstep operational semantics
  def eval1(t: Term): Option[Term] =
    t match
      case TmIf(TmTrue,t2,_)        => Some(t2)
      case TmIf(TmFalse,_,t3)       => Some(t3)
      case TmIf(t1,t2,t3)           =>
        Applicative[Option].ap3(Some(TmIf.apply))(eval1(t1), Some(t2), Some(t3))
      case TmIsZero(TmZero)         => Some(TmTrue)
      case TmIsZero(TmSucc(t:Term)) => if isNumeric(t) then Some(TmFalse) else None
      case TmIsZero(t)              => eval1(t).map(TmIsZero.apply)
      case TmPred(TmZero)           => Some(TmZero)
      case TmPred(TmSucc(t:Term))   => if isNumeric(t) then Some(t) else None
      case TmPred(t:Term)           => eval1(t).map(TmPred.apply)
      case TmSucc(t:Term)           => eval1(t).map(TmSucc.apply)
      case _                        => None

  def eval(t: Term): Option[Term] =
    eval1(t) match
      case Some(t) => eval(t)
      case None    => Some(t)

// val _ = Applicative[Option].ap(Some((x:Integer) => x + 1))(Some(2))

import Term.*

val t = TmFalse
val t2 = TmIsZero(TmIf(TmFalse,TmTrue,TmPred(TmSucc(TmZero))))
