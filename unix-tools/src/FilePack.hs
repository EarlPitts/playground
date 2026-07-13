{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImpredicativeTypes #-}

module FilePack where

import Data.Bifunctor (first)
import Data.Bits (shift, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Word (Word32)
import Data.Word8 (Word8)
import System.Posix.Types (CMode (..), FileMode)
import Text.Printf (printf)
import Text.Read (readEither)

class Encode a where
  encode :: a -> ByteString
  encode = BS.drop 4 . encodeWithSize
  encodeWithSize :: a -> ByteString
  encodeWithSize a =
    let
      s = encode a
      l = fromIntegral $ BS.length s
     in
      word32ToByteString l <> s
  {-# MINIMAL encode | encodeWithSize #-}

class Decode a where
  decode :: ByteString -> Either String a

instance Encode String where
  encode = BC.pack

instance Encode Text where
  encode = encodeUtf8

instance Encode ByteString where
  encode = id

instance Encode Word32 where
  encode = word32ToByteString
  encodeWithSize w =
    word32ToByteString 4 <> encode w

instance Decode Word32 where
  decode = byteStringToWord32

instance Encode FileMode where
  encode (CMode fmode) = encode fmode

instance (Encode a, Encode b) => Encode (a, b) where
  encode (a, b) = encodeWithSize a <> encodeWithSize b

instance {-# OVERLAPPABLE #-} (Encode a) => Encode [a] where
  encode = foldMap encode

-- instance Encode FilePack where
--   encode (FilePack a) = encode a

instance Decode FileMode where
  decode = fmap CMode . decode

instance Decode ByteString where
  decode = Right

instance Decode Text where
  decode = first show . decodeUtf8'

instance Decode String where
  decode = fmap T.unpack . decode

instance (Encode a) => Encode (FileData a) where
  encode FileData{..} =
    encodeWithSize fileName
      <> encodeWithSize fileSize
      <> encodeWithSize filePermissions
      <> encodeWithSize fileContent

data FileData a = FileData
  { fileName :: FilePath
  , fileSize :: Word32
  , filePermissions :: FileMode
  , fileContent :: a
  }
  deriving (Show, Read, Eq)

-- mkFileData ::
--   (Encode a) =>
--   FilePath ->
--   Word32 ->
--   FileMode ->
--   a ->
--   FileData
-- mkFileData path size perm cont =
--   FileData path size perm (encode cont)

newtype FilePack = FilePack [forall a. Encode a => a]

-- packFiles :: FilePack -> ByteString
-- packFiles = encode

-- unpackFiles :: ByteString -> Either String (FilePack a)
-- unpackFiles serializedData =
--   B64.decode serializedData >>= readEither . BC.unpack

word32ToBytes :: Word32 -> (Word8, Word8, Word8, Word8)
word32ToBytes word =
  let
    a = fromIntegral $ 255 .&. word
    b = fromIntegral $ 255 .&. shift word (-8)
    c = fromIntegral $ 255 .&. shift word (-16)
    d = fromIntegral $ 255 .&. shift word (-24)
   in
    (a, b, c, d)

word32ToByteString :: Word32 -> ByteString
word32ToByteString word =
  let (a, b, c, d) = word32ToBytes word
   in BS.pack [a, b, c, d]

word32FromBytes :: (Word8, Word8, Word8, Word8) -> Word32
word32FromBytes (a, b, c, d) =
  let
    a' = fromIntegral a
    b' = shift (fromIntegral b) 8
    c' = shift (fromIntegral c) 16
    d' = shift (fromIntegral d) 24
   in
    a' .|. b' .|. c' .|. d'

byteStringToWord32 :: ByteString -> Either String Word32
byteStringToWord32 bs = case BS.unpack bs of
  [a, b, c, d] -> Right $ word32FromBytes (a, b, c, d)
  _otherwise -> Left $ "Expected 4 bytes, got " <> show (BS.length bs)

--------------------------------------------------------

-- sampleFilePack :: FilePack
-- sampleFilePack =
--   FilePack
--     [ FileData "stringFile" 0 0 ("hello string" :: String)
--     -- , FileData "textFile" 0 0 ("hello text" :: Text)
--     -- , FileData "binaryFile" 0 0 ("hello bytestring" :: ByteString)
--     ]

-- testPackFile :: ByteString
-- testPackFile = packFiles sampleFilePack

-- testUnpackFile :: Either String (FilePack a)
-- testUnpackFile = unpackFiles testPackFile

-- testRoundTrip :: (Eq a) => FilePack a -> Bool
-- testRoundTrip pack =
--   Right pack == unpackFiles (packFiles pack)

showBinary :: Word8 -> String
showBinary = printf "%b"
