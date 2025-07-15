module Exercises where

-- https://github.com/alpacaaa/zero-bs-haskell

import qualified Data.Aeson as Aeson
import Data.List (groupBy, sortBy, sortOn)
import Data.List.Extra (snoc)
import Data.Ord
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Server as S

spell :: Int -> String
spell 0 = "zero"
spell 1 = "one"
spell 2 = "two"
spell 3 = "three"
spell _ = error "not covered"

conservatizer :: String -> String
conservatizer s = T.unpack $ T.replace "I'm positive" "I think" (T.pack s)

-- Handlers
helloHandler :: S.Request -> S.Response
helloHandler _ = S.stringResponse "hello"

echoHandler :: S.Request -> S.Response
echoHandler req = S.stringResponse (S.requestBody req)

caseHandler :: S.Request -> S.Response
caseHandler = S.stringResponse . spell . read . S.requestBody

manipulateHandler :: S.Request -> S.Response
manipulateHandler = S.stringResponse . conservatizer . S.requestBody

newtype Switch = Switch Bool

instance Show Switch where
  show (Switch False) = "Off"
  show (Switch True) = "On"

flipSwitch :: Switch -> Switch
flipSwitch (Switch b) = Switch (not b)

onOffHandler :: S.StatefulHandler Switch
onOffHandler = S.statefulHandler S.POST "/onoff-switch" handle
  where
    handle s _ = (flipSwitch s, S.stringResponse (show (flipSwitch s)))

increaseCounterHandler :: S.StatefulHandler Int
increaseCounterHandler = S.statefulHandler S.POST "/increase" handle
  where
    handle s _ = (succ s, S.stringResponse (show (succ s)))

getCounterHandler :: S.StatefulHandler Int
getCounterHandler = S.statefulHandler S.GET "/current-count" handle
  where
    handle s _ = (s, S.stringResponse (show s))

data Item = Item
  { model :: String,
    quantity :: Int
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

newtype Cart
  = Cart {items :: [Item]}
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON Cart where
  toJSON (Cart is) = Aeson.toJSON is

addItem :: Cart -> Item -> Cart
addItem (Cart is) i = Cart (snoc is i)

instance Semigroup Cart where
  (<>) (Cart is1) (Cart is2) = Cart $ sortBy (comparing Data.Ord.Down) (fmap merge groups)
    where
      groups = groupBy (\i1 i2 -> model i1 == model i2) (sortOn model (is1 ++ is2))
      merge l =
        let q = foldr ((+) . quantity) 0 l
         in Item (model (head l)) q

instance Monoid Cart where
  mempty = Cart []

instance Ord Item where
  compare i1 i2 =
    if quantity i1 > quantity i2
      then GT
      else LT

addToCartHandler :: S.StatefulHandler Cart
addToCartHandler = S.statefulHandler S.POST "/cart" handle
  where
    handle s req =
      let item = S.decodeJson $ S.requestBody req
       in case item of
            Left err -> (s, S.stringResponse err)
            Right i -> (Cart [i] <> s, S.jsonResponse (Cart [i] <> s))

getCartHandler :: S.StatefulHandler Cart
getCartHandler = S.statefulHandler S.GET "/cart" handle
  where
    handle cart _ = (cart, S.jsonResponse cart)

-- Server
serve :: IO ()
serve =
  S.startServer
    [ S.simpleHandler S.GET "/hello" helloHandler,
      S.simpleHandler S.POST "/echo" echoHandler,
      S.simpleHandler S.POST "/case" caseHandler,
      S.simpleHandler S.POST "/string-manipulation" manipulateHandler,
      S.handlersWithState (Switch False) [onOffHandler],
      S.handlersWithState 0 [increaseCounterHandler, getCounterHandler],
      S.handlersWithState (mempty :: Cart) [getCartHandler, addToCartHandler]
    ]
