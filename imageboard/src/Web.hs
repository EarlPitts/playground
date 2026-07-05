{-# LANGUAGE OverloadedStrings #-}

module Web (
  Handle (..),
  Config (..),
  withHandle,
  run,
) where

import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Types
import Control.Applicative (empty, (<|>))
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Database
import qualified Logger
import Lucid
import Network.HTTP.Types.Status (status404)
import Network.Wai.Parse (fileContent)
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
    [(_, pImage)] <- Scotty.files -- TODO
    let originalImage = LBS.toStrict $ fileContent pImage
    case resizeImage originalImage of
      Left _ -> do
        Scotty.redirect "/"
      Right thumbnail -> do
        tSubject <- Scotty.formParam "subject"
        tId <- liftIO $ Database.createThread (hDatabase h) (Database.CreateThread tSubject)
        liftIO $ Logger.logInfo (hLogger h) $ "Created new thread with id " <> show tId
        pId <- liftIO $ Database.createPost (hDatabase h) (Database.CreatePost pText tId True)
        liftIO $ BS.writeFile ("uploads/" <> show pId) originalImage
        liftIO $ BS.writeFile ("uploads/" <> show pId <> "_thumb") thumbnail
        Scotty.redirect "/"

  Scotty.post "/newPost/:tId" $ do
    tId <- Scotty.pathParam "tId"
    pText <- Scotty.formParam "text"
    [(_, pImage)] <- Scotty.files
    let originalImage = LBS.toStrict $ fileContent pImage
    case resizeImage originalImage of
      Left _ -> do
        void $ liftIO $ Database.createPost (hDatabase h) (Database.CreatePost pText tId False)
        Scotty.redirect $ TL.pack ("/thread/" <> show tId)
      Right thumbnail -> do
        pId <- liftIO $ Database.createPost (hDatabase h) (Database.CreatePost pText tId True)
        liftIO $ BS.writeFile ("uploads/" <> show pId) originalImage
        liftIO $ BS.writeFile ("uploads/" <> show pId <> "_thumb") thumbnail
        Scotty.redirect $ TL.pack ("/thread/" <> show tId)

  Scotty.get "/assets/style.css" $ do
    Scotty.setHeader "Content-Type" "text/css"
    Scotty.file "assets/style.css"

  Scotty.get "/uploads/:id" $ do
    fileId <- Scotty.pathParam "id"
    Scotty.setHeader "Content-Type" "image/jpeg"
    Scotty.file $ "uploads/" <> fileId

resizeImage :: ByteString -> Either () ByteString
resizeImage original = case decodeImage original of
  Left _ -> Left ()
  Right image -> Right $ LBS.toStrict $ encodeJpeg (convertImage thumbnail)
   where
    img = convertRGB8 image
    width = imageWidth img
    height = imageHeight img
    longer = max width height
    factor = div longer 200
    newWidth = round (fromIntegral width / fromIntegral factor)
    newHeight = round (fromIntegral height / fromIntegral factor)
    thumbnail = scaleBilinear newWidth newHeight img
