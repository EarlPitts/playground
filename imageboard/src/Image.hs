{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Image (
  Image (..),
  getImage,
) where

import Codec.Picture hiding (Image)
import Codec.Picture.Extra
import Codec.Picture.Metadata (Keys (Format), Metadatas, SourceFormat (..))
import qualified Codec.Picture.Metadata as M
import Codec.Picture.Types (convertImage)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Network.Wai.Parse (FileInfo (fileName), fileContent)
import Web.Scotty (File)

data Image = Image
  { iThumbPath :: FilePath
  , iPath :: FilePath
  , iThumb :: ByteString
  , iOriginal :: ByteString
  , iFilename :: String
  }

getImage :: [File LBS.ByteString] -> Maybe Image
getImage [(_, fileInfo)] = case decodeImageWithMetadata image of
  Left _ -> Nothing
  Right (decodedImage, metadata) -> do
    format <- imageFormat metadata
    iThumb <- makeThumbnail decodedImage format
    let iThumbPath = "uploads/thumb_" <> filename
        iPath = "uploads/" <> filename
        iOriginal = image
        iFilename = filename
     in Just Image{..}
 where
  image = LBS.toStrict $ fileContent fileInfo
  filename = C8.unpack $ fileName fileInfo
getImage _ = Nothing

imageFormat :: Metadatas -> Maybe SourceFormat
imageFormat ms = M.lookup Format ms

makeThumbnail :: DynamicImage -> SourceFormat -> Maybe ByteString
makeThumbnail = resizeImage 200

resizeImage :: Int -> DynamicImage -> SourceFormat -> Maybe ByteString
resizeImage size image = \case
  SourceJpeg -> Just $ LBS.toStrict $ encodeJpeg (convertImage thumbnail)
  SourcePng -> Just $ LBS.toStrict $ encodePng @PixelRGB8 (convertImage thumbnail)
  _ -> Nothing
 where
  img = convertRGB8 image
  width = imageWidth img
  height = imageHeight img
  longer = max width height
  factor = div longer size
  newWidth = round @Double (fromIntegral width / fromIntegral factor)
  newHeight = round @Double (fromIntegral height / fromIntegral factor)
  thumbnail = scaleBilinear newWidth newHeight img
