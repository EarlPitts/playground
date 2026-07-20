{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ServerMessage where

import Codec.Serialise
import Protolude

data ServerMessage = ServerMessage
  { mUsername :: ByteString
  , mText :: ByteString
  }
  deriving (Show, Eq, Generic, Serialise)
