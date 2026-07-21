{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Core (
  ServerMessage (..),
  Config (..),
  defaultConfig,
) where

import Codec.Serialise
import Protolude

data Config = Config
  { cHost :: Text
  , cPort :: Text
  }
  deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config "0.0.0.0" "5000"

data ServerMessage = ServerMessage
  { mUsername :: ByteString
  , mText :: ByteString
  }
  deriving (Show, Eq, Generic, Serialise)
