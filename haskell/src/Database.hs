{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

main :: IO ()
main = do
  conn <- open "test.db"
  execute conn "INSERT INTO test (str) VALUES (?)" ["test string 4" :: String]
  [[a :: Int]] <- query conn "select ? + ?" (40 :: Int,2 :: Int)
  print a
  r <- query_ conn "SELECT * from test" :: IO [TestField]
  traverse print r
  close conn
