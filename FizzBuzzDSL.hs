-- https://themonadreader.wordpress.com/wp-content/uploads/2014/04/fizzbuzz.pdf

data Cmd = Skip | Halt | Print String

type Program = [Cmd]

type Cont = Program -> Program

fizz :: Int -> Cont
fizz n
  | n `mod` 3 == 0 = \x -> [Print "fizz"] ++ x ++ [Halt]
  | otherwise = id

buzz :: Int -> Cont
buzz n
  | n `mod` 5 == 0 = \x -> [Print "buzz"] ++ x ++ [Halt]
  | otherwise = id

base :: Int -> Cont
base n = \x -> x ++ [Print (show n)]

fb :: Int -> Program
fb n = (base n . fizz n . buzz n) [Skip]

step :: Cmd -> String -> String
step Skip t = t
step Halt t = ""
step (Print s) t = s ++ t

eval :: Program -> String
eval = foldr step ""

fizzbuzz :: Int -> String
fizzbuzz n = eval (fb n)

fizzbuzz' :: Int -> String
fizzbuzz' n = (test 3 "fizz" . test 5 "buzz") id (show n)
  where
    test d s x
      | n `mod` d == 0 = const (s ++ x "")
      | otherwise = x
