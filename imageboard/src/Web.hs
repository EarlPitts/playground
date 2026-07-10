{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Web (
  Handle (..),
  Config (..),
  withHandle,
  run,
) where

import Control.Applicative (empty, (<|>))
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Database
import qualified Logger
import Image
import Lucid
import Network.HTTP.Types.Status (status404)
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty
import Web.View

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
  parseJSON = A.withObject "FromJSON Web.Config" $ \o ->
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
    threads <- liftIO $ Database.getThreads (hDatabase h)
    Scotty.html $ renderText (index threads)

  Scotty.get "/thread/:tId" $ do
    tId <- Scotty.pathParam "tId"
    maybeThread <- liftIO $ Database.getThreadById (hDatabase h) tId
    case maybeThread of
      Nothing -> Scotty.status status404
      Just t -> Scotty.html $ renderText (threadView t)

  Scotty.post "/newThread" $ do
    pText <- Scotty.formParam "text"
    tSubject <- Scotty.formParam "subject"
    files <- Scotty.files
    case getImage files of
      Nothing -> Scotty.redirect "/"
      Just image -> do
        liftIO $ newThread h tSubject pText image
        Scotty.redirect "/"

  Scotty.post "/newPost/:tId" $ do
    tId <- Scotty.pathParam "tId"
    pText <- Scotty.formParam "text"
    files <- Scotty.files
    let image = getImage files
    liftIO $ newPost h tId pText image
    Scotty.redirect $ TL.pack ("/thread/" <> show tId)

  Scotty.get "/assets/style.css" $ do
    Scotty.setHeader "Content-Type" "text/css"
    Scotty.file "assets/style.css"

  Scotty.get "/uploads/:filename" $ do
    path <- Scotty.pathParam "filename"
    Scotty.file $ "uploads/" <> path

newPost :: Handle -> Int64 -> T.Text -> Maybe Image -> IO ()
newPost h tId pText = \case
  Nothing -> void $ Database.createPost (hDatabase h) (Database.CreatePost pText tId Nothing)
  Just Image{..} -> do
    void $ Database.createPost (hDatabase h) (Database.CreatePost pText tId (Just iFilename))
    BS.writeFile iPath iOriginal
    BS.writeFile iThumbPath iThumb

newThread :: Handle -> T.Text -> T.Text -> Image -> IO ()
newThread h tSubject pText Image{..} = do
  tId <- Database.createThread (hDatabase h) (Database.CreateThread tSubject)
  Logger.logInfo (hLogger h) $ "Created new thread with id " <> show tId
  void $ Database.createPost (hDatabase h) (Database.CreatePost pText tId (Just iFilename))
  BS.writeFile iPath iOriginal
  BS.writeFile iThumbPath iThumb
