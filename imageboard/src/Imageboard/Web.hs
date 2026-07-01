{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Imageboard.Web where

import Control.Applicative (empty, (<|>))
import Control.Exception.Lifted (Handler (..), catches)
import Control.Monad (void)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as A
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Time as Time
import Imageboard.Database (Post (..))
import qualified Imageboard.Database as Database
import qualified Imageboard.Logger as Logger
import Lucid
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

data Config = Config
  { cPort :: Maybe Int
  , cDomain :: Maybe T.Text
  }
  deriving (Show)

instance Semigroup Config where
  (<>) c1 c2 =
    Config
      { cPort = cPort c1 <|> cPort c2
      , cDomain = cDomain c1 <|> cDomain c2
      }

instance Monoid Config where
  mempty = Config empty empty

instance A.FromJSON Config where
  parseJSON = A.withObject "FromJSON Imageboard.Web.Config" $ \o ->
    Config
      <$> o A..:? "port"
      <*> o A..:? "domain"

data Handle = Handle
  { hConfig :: Config
  , hLogger :: Logger.Handle
  , hDatabase :: Database.Handle
  }

withHandle ::
  Config ->
  Logger.Handle ->
  Database.Handle ->
  (Handle -> IO a) ->
  IO a
withHandle config logger database f =
  f $ Handle config logger database

run :: Handle -> IO ()
run h = Scotty.scotty port (app h)
 where
  port = fromMaybe 8000 $ cPort (hConfig h)

app :: Handle -> ScottyM ()
app h = do
  Scotty.get "/" $ do
    posts <- liftIO $ Database.getPosts (hDatabase h)
    Scotty.html $ renderText (mainPage posts)
  Scotty.post "/newThread" $ do
    pText <- Scotty.formParam "text"
    tSubject <- Scotty.formParam "subject"
    tId <- liftIO $ Database.createThread (hDatabase h) (Database.CreateThread tSubject)
    void $ liftIO $ Database.createPost (hDatabase h) (Database.CreatePost pText tId)
    Scotty.redirect "/"
  -- Scotty.post "/newPost" $ do
  --   pText <- Scotty.formParam "text"
  --   void $ liftIO $ Database.createPost (hDatabase h) (Database.CreatePost pText)
  --   Scotty.redirect "/"

mainPage :: [Post] -> Html ()
mainPage posts = do
  p_ "Hello!"
  form_ [method_ "post", enctype_ "multipart/form-data", action_ "/newThread"] $ do
    -- input_ [name_ "name", placeholder_ "Anonymous", type_ "text"]
    input_ [name_ "subject", placeholder_ "Subject", type_ "text"]
    textarea_ [name_ "text"] ""
    -- input_ [name_ "image", type_ "file"]
    button_ "Post"
  traverse_ postView posts

postView :: Post -> Html ()
postView Post{..} = do
  p_ "Thread Id: " <> toHtml (show pThreadId)
  p_ "Post Id: " <> toHtml (show pId)
  p_ "Text: " <> toHtml pText
