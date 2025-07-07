package ULCPlayground

import ULC.*
import ULC.Term.*
import ULC.RawTerm.*

val rt = "\\x.x"
val rt2 = "\\x.\\y.x"
val rt3 = "\\x.\\y.(x y)"
val rt4 = "\\x.\\y.\\z.((x y) z)"
val rt5 = "(\\x.x \\x.x)"

val t = TmApp(TmAbs(TmVar(0,1)), (TmAbs(TmVar(0,1))))
val t2 = TmApp(TmAbs(TmVar(1,2)), (TmAbs(TmVar(0,2))))
val t3 = TmApp(TmAbs(TmAbs(TmApp(TmVar(2,3), TmVar(0,3)))), TmAbs(TmVar(0,2)))

// Basic combinators
// val id = TmAbs(TmVar(0,1))
// val const = TmAbs(TmAbs(TmVar(1,2)))
// val comp = TmAbs(TmAbs(TmAbs(TmApp(TmVar(2,3),TmApp(TmVar(1,3),TmVar(0,3))))))

def id(x: Term) = TmApp(TmAbs(TmVar(0,1)), x)
def const(x: Term, y: Term) = TmApp(TmApp(TmAbs(TmAbs(TmVar(1,2))), y), x)
