module Arrow where

import Control.Arrow
import Control.Category qualified as Cat
import Control.Monad
import Data.List
import Data.Maybe
import System.Random

newtype Circuit a b = Circuit {unCircuit :: a -> (Circuit a b, b)}

instance Cat.Category Circuit where
  id = Circuit $ \a -> (Cat.id, a)
  (.) = dot
    where
      (Circuit cir2) `dot` (Circuit cir1) = Circuit $ \a ->
        let (cir1', b) = cir1 a
            (cir2', c) = cir2 b
        in  (cir2' `dot` cir1', c)
