{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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

instance Encode String where
  encode = BC.pack

instance Encode Text where
  encode = encodeUtf8

instance Encode ByteString where
  encode = id

instance Encode Word32 where
  encode = word32ToByteString

instance Decode Word32 where
  decode = byteStringToWord32

instance Decode ByteString where
  decode = Right

instance Decode Text where
  decode = first show . decodeUtf8'

instance Decode String where
  decode = fmap T.unpack . decode

class Decode a where
  decode :: ByteString -> Either String a

data FileData = FileData
  { fileName :: FilePath
  , fileSize :: Word32
  , filePermissions :: FileMode
  , fileContent :: ByteString
  }
  deriving (Show, Read, Eq)

mkFileData ::
  (Encode a) =>
  FilePath ->
  Word32 ->
  FileMode ->
  a ->
  FileData
mkFileData path size perm cont =
  FileData path size perm (encode cont)

newtype FilePack = FilePack
  {getPackedFiles :: [FileData]}
  deriving (Show, Eq, Read)

packFiles :: FilePack -> ByteString
packFiles = B64.encode . BC.pack . show

unpackFiles :: ByteString -> Either String FilePack
unpackFiles serializedData =
  B64.decode serializedData >>= readEither . BC.unpack

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

sampleFilePack :: FilePack
sampleFilePack =
  FilePack
    [ mkFileData "stringFile" 0 0 ("hello string" :: String)
    , mkFileData "textFile" 0 0 ("hello text" :: Text)
    , mkFileData "binaryFile" 0 0 ("hello bytestring" :: ByteString)
    ]

testPackFile :: ByteString
testPackFile = packFiles sampleFilePack

testUnpackFile :: Either String FilePack
testUnpackFile = unpackFiles testPackFile

testRoundTrip :: FilePack -> Bool
testRoundTrip pack =
  Right pack == unpackFiles (packFiles pack)

showBinary :: Word8 -> String
showBinary = printf "%b"
