{-# LANGUAGE OverloadedStrings #-}

module FilePack where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import System.Posix.Types (CMode (..), FileMode)
import Text.Read (readEither)

data FileContents
  = StringFileContents String
  | TextFileContents Text
  | ByteStringFileContents ByteString
  deriving (Show, Eq, Read)

data FileData = FileData
  { fileName :: FilePath
  , fileSize :: Word32
  , filePermissions :: FileMode
  , fileContent :: FileContents
  }
  deriving (Show, Read, Eq)

newtype FilePack = FilePack
  {getPackedFiles :: [FileData]}
  deriving (Show, Eq, Read)

packFiles :: FilePack -> ByteString
packFiles = B64.encode . BC.pack . show

unpackFiles :: ByteString -> Either String FilePack
unpackFiles serializedData =
  B64.decode serializedData >>= readEither . BC.unpack

--------------------------------------------------------

sampleFilePack :: FilePack
sampleFilePack =
  FilePack
    [ FileData "stringFile" 0 0 $ StringFileContents "hello string"
    , FileData "textFile" 0 0 $ TextFileContents "hello text"
    , FileData "binaryFile" 0 0 $ ByteStringFileContents "hello bytestring"
    ]

testPackFile :: ByteString
testPackFile = packFiles sampleFilePack

testUnpackFile :: Either String FilePack
testUnpackFile = unpackFiles testPackFile

testRoundTrip :: FilePack -> Bool
testRoundTrip pack =
  Right pack == unpackFiles (packFiles pack)
