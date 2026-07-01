{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Imageboard.Database where

import Control.Exception (Exception, catch, throwIO)
import Control.Exception.Base (bracket)
import Control.Monad (replicateM, void)
import qualified Data.Aeson as Aeson
import Data.Int (Int64)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NL
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
    \    thread_id INTEGER NOT NULL REFERENCES threads(id) ON DELETE CASCADE, \
    \    text TEXT NOT NULL \
    \)"
  SQLite.execute_
    (hConn h)
    "CREATE TABLE IF NOT EXISTS threads ( \
    \    id INTEGER PRIMARY KEY NOT NULL, \
    \    subject TEXT NOT NULL \
    \)"

-- SQLite.execute_
--   (hConn h)
--   "CREATE UNIQUE INDEX IF NOT EXISTS users_address ON users(address)"
-- SQLite.execute_
--   (hConn h)
--   "CREATE UNIQUE INDEX IF NOT EXISTS mails_to ON mails(id, \"to\")"

data CreatePost = CreatePost
  { cpText :: T.Text
  , cpThreadId :: Int64
  }
  deriving (Show)

data CreateThread = CreateThread
  { ctSubject :: T.Text
  }
  deriving (Show)

data ThreadPostRow = ThreadPostRow
  { tprThreadId :: Int64
  , tprSubject :: T.Text
  , tprPostId :: Int64
  , tprPostThreadId :: Int64
  , tprPostText :: T.Text
  }

data Post = Post
  { pId :: Int64
  , pThreadId :: Int64
  , pText :: T.Text
  }
  deriving (Show)

data Thread = Thread
  { tId :: Int64
  , tSubject :: T.Text
  , tPosts :: NonEmpty Post
  }
  deriving (Show)

instance SQLite.FromRow Post where
  fromRow =
    Post
      <$> SQLite.field
      <*> SQLite.field
      <*> SQLite.field

instance SQLite.FromRow ThreadPostRow where
  fromRow =
    ThreadPostRow
      <$> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field

createThread :: Handle -> CreateThread -> IO Int64
createThread h CreateThread{..} = do
  SQLite.execute
    (hConn h)
    "INSERT INTO threads (subject) values (?)"
    (Only ctSubject)
  SQLite.lastInsertRowId (hConn h)

getThreads :: Handle -> IO [Thread]
getThreads h = do
  rows <-
    SQLite.query_
      (hConn h)
      "SELECT t.id, t.subject, p.id, p.thread_id, p.text \
      \FROM threads t \
      \JOIN posts p ON p.thread_id = t.id \
      \ORDER BY t.id, p.id"
  pure $ groupPosts rows

groupPosts :: [ThreadPostRow] -> [Thread]
groupPosts [] = []
groupPosts (r : rs) = t : groupPosts rest
 where
  (same, rest) = span (\row -> tprThreadId r == tprThreadId row) rs
  op = Post{pId = tprPostId r, pThreadId = tprThreadId r, pText = tprPostText r}
  ps = fmap (\row -> Post{pId = tprPostId row, pThreadId = tprThreadId row, pText = tprPostText row}) same
  t = Thread{tId = tprThreadId r, tSubject = tprSubject r, tPosts = op NL.:| ps}

createPost :: Handle -> CreatePost -> IO Int64
createPost h CreatePost{..} = do
  SQLite.execute
    (hConn h)
    "INSERT INTO posts (thread_id, text) values (?,?)"
    (cpThreadId, cpText)
  SQLite.lastInsertRowId (hConn h)

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
