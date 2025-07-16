type Value = Int

data Exp =
  Plus Exp Exp
  | Num Int
  deriving (Show)

eval :: Exp -> Value
              -- apply      eval
eval (Plus x y) = (+) (eval x) (eval y)
eval (Num x) = x
