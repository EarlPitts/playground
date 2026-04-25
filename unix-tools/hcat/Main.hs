module Main where

import HCat
import System.Console.Terminal.Size (Window (..), size)

-- size :: IO (Maybe (Window Int))
-- size = pure (Just (Window 2 2))

main :: IO ()
main = runHCat size
