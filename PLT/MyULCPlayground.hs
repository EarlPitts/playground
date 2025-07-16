module ULCPlayground where

import Data.List

import MyULC

test1 = App (Abs (Var 0 1)) (Abs (Var 0 1)) -- id
test2 = App (Abs (Var 1 2)) (Abs (Var 0 2))
test3 = App (Abs (Abs (App (Var 2 3) (Var 0 3)))) (Abs (Var 0 2))

-- testCtx = zip (singleton <$> ['a' .. 'z']) (repeat NameBind)

tru = Abs (Abs (Var 1 2))
fls = Abs (Abs (Var 0 2))

-- if' = Abs (Abs (Abs (App (App (Var 2 3) (Var 1 3)) (Var 0 3))))
if' g x y = g `app` x `app` y

app = App

pair x y = App (App (Abs (Abs (Abs (App (App (Var 0 3) (Var 2 3)) (Var 1 3))))) x) y
fst' p = app p tru
snd' p = app p fls

test4  = App (App tru tru) tru
test5 = (tru `app` fls `app` tru) `app` tru `app` fls
t = if' fls fls tru

z  = Abs (Abs (Var 0 2))
scc = Abs (Abs (Abs (App (Var 1 2) (App (App (Var 2 2) (Var 1 2)) (Var 0 2)))))

c1 = Abs (Abs (App (Var 1 2) (Var 0 2)))
c2 = Abs (Abs (App (Var 1 2) (App (Var 1 2) (Var 0 2))))

plus m n s z = m `app` s `app` (n `app` s `app` z)

evalShow = restoreNames [] . eval

-- evalShow t = case show [] <$> eval [] t of
--   Just tm -> tm
--   Nothing -> "Bad"
