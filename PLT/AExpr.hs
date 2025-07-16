module AExpr where

data Term =
    TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Eq, Show)

isNumericVal :: Term -> Bool
isNumericVal TmZero      = True
isNumericVal (TmSucc t1) = isNumericVal t1
isNumericVal _           = False

isVal :: Term -> Bool
isVal TmTrue  = True
isVal TmFalse = True
isVal t       = isNumericVal t

-- Evaluation using small-step semantics
eval1 :: Term -> Maybe Term
eval1 (TmIf TmTrue t2 t3)     = Just t2
eval1 (TmIf TmFalse t2 t3)    = Just t3
eval1 (TmIf t1 t2 t3)         = TmIf <$> eval1 t1 <*> pure t2 <*> pure t3
eval1 (TmSucc t1)             = TmSucc <$> eval1 t1
eval1 (TmPred TmZero)         = Just TmZero
eval1 (TmPred (TmSucc nv1))   = if isNumericVal nv1 then pure nv1 else Nothing
eval1 (TmPred t1)             = TmPred <$> eval1 t1
eval1 (TmIsZero TmZero)       = Just TmTrue
eval1 (TmIsZero (TmSucc nv1)) = if isNumericVal nv1 then pure TmFalse else Nothing
eval1 (TmIsZero t1)           = TmIsZero <$> eval1 t1
eval1 _                       = Nothing

eval :: Term -> Maybe Term
eval t = case eval1 t of
  Nothing -> Just t
  Just t' -> eval t'
