module Docker where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified Socket

type RequestBuilder = Text -> HTTP.Request

data Service = Service
  { createContainer :: ContainerCreateOptions -> IO ContainerID,
    startContainer :: ContainerID -> IO (),
    containerStatus :: ContainerID -> IO ContainerStatus
  }

mkService :: IO Service
mkService = do
  manager <- Socket.newManager "/run/docker.sock"
  let mkReq = \path ->
        HTTP.defaultRequest
          & HTTP.setRequestManager manager
          & HTTP.setRequestPath (encodeUtf8 $ "/v1.40/containers/" <> path)

  pure
    $ Service
      { createContainer = createContainer_ mkReq,
        startContainer = startContainer_ mkReq,
        containerStatus = containerStatus_ mkReq
      }

newtype Image = Image Text deriving (Show, Eq)

newtype ContainerID = ContainerID Text deriving (Show, Eq)

instance Aeson.FromJSON ContainerID where
  parseJSON = Aeson.withObject "ContrainerID" $ \obj ->
    ContainerID <$> obj .: "Id"

data ContainerStatus
  = ContainerRunning
  | ContainerFinished ContainerExitCode
  | ContainerOther Text
  deriving (Show, Eq)

imageToText :: Docker.Image -> Text
imageToText (Docker.Image t) = t

containerIdToText :: ContainerID -> Text
containerIdToText (ContainerID i) = i

newtype ContainerExitCode = ContainerExitCode Int deriving (Show, Eq)

data ContainerCreateOptions = ContainerCreateOptions
  { image :: Image
  }
  deriving (Show, Eq)

createContainer_ :: RequestBuilder -> ContainerCreateOptions -> IO ContainerID
createContainer_ mkReq options = do
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
        mkReq "create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body

  res <- HTTP.httpBS request
  parseResponse res

startContainer_ :: RequestBuilder -> ContainerID -> IO ()
startContainer_ mkReq cid = do
  let path = containerIdToText cid <> "/start"
      request = mkReq path & HTTP.setRequestMethod "POST"

  void $ HTTP.httpBS request

containerStatus_ :: RequestBuilder -> ContainerID -> IO ContainerStatus
containerStatus_ mkReq cid = do
  let path = containerIdToText cid <> "/status"
      request = mkReq path & HTTP.setRequestMethod "GET"

  resp <- HTTP.httpBS request
  traceShowIO resp
  undefined

parseResponse :: (Aeson.FromJSON a) => HTTP.Response ByteString -> IO a
parseResponse res = do
  case Aeson.eitherDecodeStrict (HTTP.getResponseBody res) of
    Left e -> throwString e
    Right status -> pure status
