module ContravariantFunctor where

class Contravariant f where
  contramap :: (b -> a) -> f a -> f b

newtype Predicate a = Predicate { runPredicate :: a -> Bool }


a = Predicate (\x -> True)
b = runPredicate a 2

instance Contravariant Predicate where
  contramap f fa = Predicate $ runPredicate fa . f
