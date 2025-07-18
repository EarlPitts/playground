module TypelevelStuff.GADT where

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

import Data.Foldable
import Data.Kind (Constraint, Type)
import Data.Map
import Data.Type.Equality
import Data.Void

-- data Expr
--   = I Int
--   | B -- boolean constants
--   | Add Expr Expr
--   | Mul Expr Expr
--   | Eq Expr Expr -- equality test

data Expr' a where
  I' :: Int -> Expr' Int
  B' :: Bool -> Expr' Bool
  Add' :: Expr' Int -> Expr' Int -> Expr' Int
  Mul' :: Expr' Int -> Expr' Int -> Expr' Int
  Eq' :: (Eq a) => Expr' a -> Expr' a -> Expr' Bool

eval :: Expr' a -> a
eval (I' n) = n
eval (B' b) = b
eval (Add' e e') = eval e + eval e'
eval (Mul' e e') = eval e + eval e'
eval (Eq' e e') = eval e == eval e'

------------------

-- data Ize = Bize | Hoze

data Malac = Malac Int Int

type X a = Either a a

x :: X Int
x = Left 3

-- type F (Maybe a) = Set a
-- type F ([] a) = Vector a

class IsSimple a

instance IsSimple Bool

instance IsSimple Int

class HasInt a

instance HasInt Int

instance (HasInt a) => HasInt [a]

instance (HasInt a) => HasInt (Map a b)

-- instance (HasInt b) => HasInt (Map a b)

data T a where
  D1 :: Int -> T String
  D2 :: T Bool
  D3 :: (a, a) -> T [a]

z :: T [Int]
z = D3 (1, 2)

data Expr'' a = IntExpr Int | BoolExpr Bool

intExpr :: Int -> Expr'' Int
intExpr = IntExpr

boolExpr :: Bool -> Expr'' Int
boolExpr = BoolExpr

data Expr a where
  I :: Int -> Expr Int
  B :: Expr Bool -- boolean constants
  Add :: (Expr a) -> (Expr a) -> Expr a

deriving instance (Show a) => Show (Expr a)

h = I 2

j = l

l :: Expr Int
l = I 2

data T' a b where
  Default :: forall a. a -> T' a a
  Raise :: T' a a
  ReturnNone :: T' a (Maybe a)

kecske :: T' Int (Maybe Int)
kecske = ReturnNone

kakas :: forall a b. a -> b -> (,) a b
kakas a b = (a, b)

-- data StringableType = forall a. (Show a) => StringableType a

-- data Stringable where
--   Stringable :: a -> (a -> String) -> Stringable
--
-- data Stringable = forall a. Stringable a (a -> String)

data Stringable = forall a. (Show a) => Stringable a

strs :: [Stringable]
strs = [Stringable 1, Stringable "fdjsk"]

printStringable :: Stringable -> IO ()
printStringable (Stringable a) = print a

a :: IO ()
a = traverse_ printStringable strs

data Adat where
  Egy :: (Monoid a) => a -> Adat
  Ketto :: (Num a) => a -> Adat

data Adat' = forall a. (Monoid a) => Egy' a | forall a. (Num a) => Ketto' a

jkfdsl = Egy' [12, 3]

jkfdsl' = Ketto' 12

data Any where
  Any :: a -> Any

xfdjsl = Any 'a'

xfdjsl' = [Any 'a', Any 2]

data User a where
  UserByName :: {userFirst :: String, userLast :: String} -> User String
  UserByID :: {userID :: Int} -> User Int

usersWithFirstName :: String -> [User String] -> [User String]
usersWithFirstName firstName = Prelude.filter ((== firstName) . userFirst)

users :: [User String]
users = undefined

-- fjsdalk :: Either () Bool
-- fjsdalk = Left ()
-- fjsdalk = Right False
-- fjsdalk = Right True

-- fjsdalk :: (,) () Bool
-- fjsdalk = ((), True)
-- fjsdalk = ((), False)

fjsdalk :: (->) Bool ()
fjsdalk _ = ()

data Ize = forall a. Ize a | Bigyo Int
