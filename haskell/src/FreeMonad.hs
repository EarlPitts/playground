module FreeMonad where

import Control.Monad.Free

data Console a = PrintLine String a | GetLine (String -> a) deriving (Functor)

echo :: Console (Console ())
echo = GetLine (\line -> PrintLine line ())

readLine :: Free Console String
readLine = liftF (GetLine id)

printLine :: String -> Free Console ()
printLine line = liftF (PrintLine line ())

echo' :: Free Console ()
echo' = do
  line <- readLine
  printLine line

runConsole :: Free Console a -> IO a
runConsole =
  iterM
    ( \op -> case op of
        GetLine f -> getLine >>= f
        PrintLine line a -> putStrLn line >> a
    )
