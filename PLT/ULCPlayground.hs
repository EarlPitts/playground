module ULCPlayground where

import Data.List

import ULC

test1 = (TmApp (TmAbs "x" (TmVar 0 1)) (TmAbs "y" (TmVar 0 1)))
test2 = (TmApp (TmAbs "x" (TmVar 1 2)) (TmAbs "y" (TmVar 0 2)))
test3 = (TmApp (TmAbs "x" (TmAbs "y" (TmApp (TmVar 2 3) (TmVar 0 3)))) (TmAbs "y" (TmVar 0 2)))

testCtx = zip (singleton <$> ['a' .. 'z']) (repeat NameBind)

tru = (TmAbs "x" (TmAbs "y" (TmVar 1 2)))
fls = (TmAbs "x" (TmAbs "y" (TmVar 0 2)))

if' = (TmAbs "b" (TmAbs "x" (TmAbs "y" (TmApp (TmApp (TmVar 2 3) (TmVar 1 3)) (TmVar 0 3)))))

app t1 t2 = (TmApp t1 t2)

pair x y = (TmApp (TmApp (TmAbs "fst" (TmAbs "snd" (TmAbs "b" (TmApp (TmApp (TmVar 0 3) (TmVar 2 3)) (TmVar 1 3))))) x) y)
fst' p = app p tru
snd' p = app p fls

test4  = (TmApp (TmApp tru tru) tru)
test5 = (tru `app` fls `app` tru) `app` tru `app` fls

z  = (TmAbs "s" (TmAbs "z" (TmVar 0 2)))
c1 = (TmAbs "s" (TmAbs "z" (TmApp (TmVar 1 2) (TmVar 0 2))))
c2 = (TmAbs "s" (TmAbs "z" (TmApp (TmVar 1 2) (TmApp (TmVar 1 2) (TmVar 0 2)))))

evalShow t = case showTm [] <$> eval [] t of
  Just tm -> tm
  Nothing -> "Bad"
