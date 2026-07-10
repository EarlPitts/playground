{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Web (
  Handle (..),
  Config (..),
  withHandle,
  run,
) where

import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Metadata (Keys (Format), Metadatas, SourceFormat (..))
import qualified Codec.Picture.Metadata as M
import Codec.Picture.Types
import Control.Applicative (empty, (<|>))
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (splitOn)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Database
import Debug.Trace (traceShowId)
import qualified Logger
import Lucid
import Network.HTTP.Types.Status (status404)
import Network.Wai.Parse (FileInfo (fileName), fileContent)
import System.Directory (listDirectory)
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
        filename = C8.unpack $ fileName pImage
    case decodeImageWithMetadata originalImage of
      Left _ -> Scotty.redirect "/"
      Right (image, metadata) -> do
        let (Just format) = imageFormat metadata
        if format `elem` [SourceJpeg, SourcePng]
          then do
            let thumbnail = makeThumbnail image format
            tSubject <- Scotty.formParam "subject"
            tId <- liftIO $ Database.createThread (hDatabase h) (Database.CreateThread tSubject)
            liftIO $ Logger.logInfo (hLogger h) $ "Created new thread with id " <> show tId
            liftIO $ Database.createPost (hDatabase h) (Database.CreatePost pText tId (Just filename))
            liftIO $ BS.writeFile ("uploads/" <> filename) originalImage
            liftIO $ BS.writeFile ("uploads/" <> "thumb_" <> filename) thumbnail
            Scotty.redirect "/"
          else Scotty.redirect "/"

  Scotty.post "/newPost/:tId" $ do
    tId <- Scotty.pathParam "tId"
    pText <- Scotty.formParam "text"
    [(_, pImage)] <- Scotty.files
    let originalImage = LBS.toStrict $ fileContent pImage
        filename = C8.unpack $ fileName pImage
    case decodeImageWithMetadata originalImage of
      Left _ -> do
        void $ liftIO $ Database.createPost (hDatabase h) (Database.CreatePost pText tId Nothing)
        Scotty.redirect $ TL.pack ("/thread/" <> show tId)
      Right (image, metadata) -> do
        let (Just format) = imageFormat metadata
        if format `elem` [SourceJpeg, SourcePng]
          then do
            let thumbnail = makeThumbnail image format
            liftIO $ Database.createPost (hDatabase h) (Database.CreatePost pText tId (Just filename))
            liftIO $ BS.writeFile ("uploads/" <> filename) originalImage
            liftIO $ BS.writeFile ("uploads/" <> "thumb_" <> filename) thumbnail
            Scotty.redirect $ TL.pack ("/thread/" <> show tId)
          else Scotty.redirect "/"

  Scotty.get "/assets/style.css" $ do
    Scotty.setHeader "Content-Type" "text/css"
    Scotty.file "assets/style.css"

  Scotty.get "/uploads/:filename" $ do
    path <- Scotty.pathParam "filename"
    Scotty.file $ "uploads/" <> path

imageFormat :: Metadatas -> Maybe SourceFormat
imageFormat ms = M.lookup Format ms

makeThumbnail :: DynamicImage -> SourceFormat -> ByteString
makeThumbnail = resizeImage 200

resizeImage :: Int -> DynamicImage -> SourceFormat -> ByteString
resizeImage size image = \case
  SourceJpeg -> LBS.toStrict $ encodeJpeg (convertImage thumbnail)
  SourcePng -> LBS.toStrict $ encodePng @PixelRGB8 (convertImage thumbnail)
 where
  img = convertRGB8 image
  width = imageWidth img
  height = imageHeight img
  longer = max width height
  factor = div longer size
  newWidth = round (fromIntegral width / fromIntegral factor)
  newHeight = round (fromIntegral height / fromIntegral factor)
  thumbnail = scaleBilinear newWidth newHeight img
