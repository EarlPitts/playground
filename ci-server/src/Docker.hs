module Docker where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Simple as HTTP
import Network.HTTP.Client.Conduit (Manager)
import RIO

data Service = Service
  { createContainer :: ContainerCreateOptions -> IO ContainerID,
    startContainer :: ContainerID -> IO ()
  }

mkService :: Manager -> Service
mkService manager =
  Service
    { createContainer = createContainer_ manager,
      startContainer = startContainer_ manager
    }

newtype Image = Image Text deriving (Show, Eq)

newtype ContainerID = ContainerID Text deriving (Show, Eq)

instance Aeson.FromJSON ContainerID where
  parseJSON = Aeson.withObject "ContrainerID" $ \obj ->
    ContainerID <$> obj .: "Id"

imageToText :: Docker.Image -> Text
imageToText (Docker.Image t) = t

containerIdToText :: ContainerID -> Text
containerIdToText (ContainerID i) = i

newtype ContainerExitCode = ContainerExitCode Int deriving (Show, Eq)

data ContainerCreateOptions = ContainerCreateOptions
  { image :: Image
  }
  deriving (Show, Eq)

createContainer_ :: Manager -> ContainerCreateOptions -> IO ContainerID
createContainer_ manager options = do
  let image = imageToText options.image
      body =
        Aeson.object
          [ ("Image", Aeson.toJSON image),
            ("Tty", Aeson.toJSON True),
            ("Labels", Aeson.object [("ci", "")]),
            ("Cmd", "echo hello"),
            ("EntryPoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
          ]
      request =
        HTTP.defaultRequest
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestManager manager
          & HTTP.setRequestPath "/v1.40/containers/create"
          & HTTP.setRequestBodyJSON body

  res <- HTTP.httpBS request
  parseResponse res

startContainer_ :: Manager -> ContainerID -> IO ()
startContainer_ manager cid = do
  let path = "/v1.40/containers/" <> containerIdToText cid <> "/start"
      request =
        HTTP.defaultRequest
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestManager manager
          & HTTP.setRequestPath (encodeUtf8 path)

  void $ HTTP.httpBS request

parseResponse :: (Aeson.FromJSON a) => HTTP.Response ByteString -> IO a
parseResponse res = do
  case Aeson.eitherDecodeStrict (HTTP.getResponseBody res) of
    Left e -> throwString e
    Right status -> pure status
