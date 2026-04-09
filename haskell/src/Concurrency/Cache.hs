module Concurrency.Cache where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Aeson
import Data.IORef
import Data.Map
import Data.Maybe (fromMaybe)

import qualified Data.ByteString as BS

type Catalog = Map String Int
type Product = String
type Price = Int

updateCache :: IORef Catalog -> IO ()
updateCache catalogRef =
  getCatalog >>= writeIORef catalogRef

getCatalog :: IO Catalog
getCatalog = do
  catalogStr <- BS.readFile "catalog.json"
  throwDecodeStrict catalogStr

priceOrder :: (Product -> Price) -> [Product] -> Price
priceOrder getPrice products = sum $ getPrice <$> products

doStuff :: IORef Catalog -> IO ()
doStuff catalogRef = do
  catalog <- readIORef catalogRef
  let getPrice p = fromMaybe 0 (Data.Map.lookup p catalog)
  let totalPrice = priceOrder getPrice ["p1", "p2"]
  print totalPrice

main :: IO ()
main = do
  catalogRef <- getCatalog >>= newIORef
  concurrently_
    (forever $ updateCache catalogRef >> threadDelay 5000000)
    (forever $ doStuff catalogRef >> threadDelay 1000000)
