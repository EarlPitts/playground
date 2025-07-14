module Exercises where

-- https://github.com/alpacaaa/zero-bs-haskell

import qualified Data.Text as T
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

-- Server
serve :: IO ()
serve =
  S.startServer
    [ S.simpleHandler S.GET "/hello" helloHandler,
      S.simpleHandler S.POST "/echo" echoHandler,
      S.simpleHandler S.POST "/case" caseHandler,
      S.simpleHandler S.POST "/string-manipulation" manipulateHandler,
      S.handlersWithState (Switch False) [onOffHandler],
      S.handlersWithState 0 [increaseCounterHandler, getCounterHandler]
    ]
