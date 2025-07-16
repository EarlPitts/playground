module ULC where

import Data.List

data Term =
    TmVar Int Int -- (de Bruijn index, context size)
  | TmAbs String Term -- Stores the name of the variable for printing
  | TmApp Term Term
  deriving (Eq, Show)

type Context = [(String, Binding)]

data Binding = NameBind deriving (Eq, Show)

showTm :: Context -> Term -> String
showTm ctx (TmAbs x t1) = "(lambda " ++ x' ++ ". " ++ showTm ctx' t1 ++ ")"
  where (ctx', x') = pickFreshName ctx x
showTm ctx (TmApp t1 t2) = "(" ++ showTm ctx t1 ++ " " ++ showTm ctx t2 ++ ")"
showTm ctx (TmVar x n) =
  if ctxlength ctx == n
  then indexToName ctx x
  else error $ "Bad index: DeBroijn: " ++ show x ++ " Context size: " ++ show n ++ " Context: " ++ show ctx

-- Shifts each free variable's de Broijn index up by one
termShift :: Int -> Term -> Term
termShift d = walk 0
  where walk c (TmVar x n)   = if x >= c then TmVar (x + d) (n + d) else TmVar x (n + d)
        walk c (TmAbs x t1)  = TmAbs x (walk (c + 1) t1)
        walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)

-- Substitutes the variable j in some term with s
-- e.g.: [b -> a](b (λx.λy.b))
termSubst :: Int -> Term -> Term -> Term
termSubst j s = walk 0
  where walk c (TmVar x n)   = if x == j + c then termShift c s else TmVar x n
        walk c (TmAbs x t1)  = TmAbs x (walk (c+1) t1)
        walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)

-- ### Representation ###

-- Used for printing, finds the name of the variable in the context
indexToName :: Context -> Int -> String
indexToName ctx n = fst $ ctx !! n

ctxlength = length

-- Renames the variable if it's already in the context
pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx x = if hasName ctx x
                      then pickFreshName ctx (x ++ "'")
                      else ((x, NameBind) : ctx, x)

hasName :: Context -> String -> Bool
hasName ctx x = elem x names
  where (names, _) = unzip ctx

-- ### Evaluation ###

-- Only lambda abstractions are considered values
isVal :: Context -> Term -> Bool
isVal ctx (TmAbs _ _) = True
isVal ctx _           = False

-- Beta-reduction
termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

-- Single-step evaluation (small-step semantics)
eval1 :: Context -> Term -> Maybe Term
eval1 ctx (TmApp (TmAbs x t12) v2@(TmAbs _ _)) = Just $ termSubstTop v2 t12
eval1 ctx (TmApp v1@(TmAbs _ _) t2) = let t2' = eval1 ctx t2 in TmApp v1 <$> t2'
eval1 ctx (TmApp t1 t2) = let t1' = eval1 ctx t1 in TmApp <$> t1' <*> pure t2
eval1 _   _ = Nothing

eval :: Context -> Term -> Maybe Term
eval ctx t = case t' of
              Nothing -> Just t
              Just t' -> eval ctx t'
  where t' = eval1 ctx t
