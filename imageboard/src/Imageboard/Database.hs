{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Imageboard.Database where

import Control.Exception (Exception, catch, throwIO)
import Control.Exception.Base (bracket)
import Control.Monad (replicateM, void)
import qualified Data.Aeson as Aeson
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Time as Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Database.SQLite.Simple (Connection, Only (..))
import qualified Database.SQLite.Simple as SQLite
import System.Random (randomRIO)

data Error
  = Constraint String
  | NotFound String

instance Show Error where
  show (Constraint msg) = msg
  show (NotFound msg) = msg

instance Exception Error

data Config = Config
  { cDbPath :: Last String
  }
  deriving (Show)

instance Semigroup Config where
  (<>) (Config l) (Config r) = Config (l <> r)

instance Monoid Config where
  mempty = Config mempty

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "FromJSON Config" $ \o ->
    Config <$> o Aeson..: "db_path"

data Handle = Handle
  { hConfig :: Config
  , hConn :: Connection
  }

-- TODO bracket this
withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config = bracket (new config) close

close :: Handle -> IO ()
close h = do
  SQLite.close (hConn h)
  putStrLn "closed db"

new :: Config -> IO Handle
new config = do
  conn <- SQLite.open connString
  let handle = Handle config conn
  createTables handle
  putStrLn "created table"
  pure handle
 where
  connString = fromMaybe "" $ getLast $ cDbPath config

createTables :: Handle -> IO ()
createTables h = do
  SQLite.execute_
    (hConn h)
    "CREATE TABLE IF NOT EXISTS posts ( \
    \    id INTEGER PRIMARY KEY, \
    \    text TEXT NOT NULL \
    \)"
  putStrLn "created"

-- SQLite.execute_
--   (hConn h)
--   "CREATE UNIQUE INDEX IF NOT EXISTS users_address ON users(address)"
-- SQLite.execute_
--   (hConn h)
--   "CREATE TABLE IF NOT EXISTS mails ( \
--   \    id TEXT PRIMARY KEY NOT NULL, \
--   \    \"from\" TEXT NOT NULL, \
--   \    \"to\" TEXT NOT NULL, \
--   \    subject TEXT NOT NULL, \
--   \    source TEXT NOT NULL \
--   \)"
-- SQLite.execute_
--   (hConn h)
--   "CREATE UNIQUE INDEX IF NOT EXISTS mails_to ON mails(id, \"to\")"

data CreatePost = CreatePost
  { cText :: T.Text
  }
  deriving (Show)

data Post = Post
  { pId :: Int64
  , pText :: T.Text
  }
  deriving (Show)

instance SQLite.FromRow Post where
  fromRow =
    Post
      <$> SQLite.field
      <*> SQLite.field

createPost :: Handle -> CreatePost -> IO Int
createPost h CreatePost{..} = do
  SQLite.execute
    (hConn h)
    "INSERT INTO posts (text) values (?)"
    (Only cText)
  fromIntegral <$> SQLite.lastInsertRowId (hConn h)

getPosts :: Handle -> IO [Post]
getPosts h =
  SQLite.query_
    (hConn h)
    "SELECT * from posts"

getPostById :: Handle -> Int -> IO [Post]
getPostById h pId =
  SQLite.query
    (hConn h)
    "SELECT * from posts where id = ?"
    (Only (fromIntegral pId :: Int64))
