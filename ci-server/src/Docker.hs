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
    containerStatus :: ContainerID -> IO ContainerStatus,
    createVolume :: IO Volume
  }

mkService :: IO Service
mkService = do
  manager <- Socket.newManager "/run/docker.sock"
  let mkReq = \path ->
        HTTP.defaultRequest
          & HTTP.setRequestManager manager
          & HTTP.setRequestPath (encodeUtf8 $ "/v1.40/" <> path)

  pure
    $ Service
      { createContainer = createContainer_ mkReq,
        startContainer = startContainer_ mkReq,
        containerStatus = containerStatus_ mkReq,
        createVolume = createVolume_ mkReq
      }

newtype Volume = Volume Text deriving (Show, Eq)

instance Aeson.FromJSON Volume where
  parseJSON = Aeson.withObject "CreateVolume" $ \obj ->
    Volume <$> obj .: "Name"

newtype Image = Image Text deriving (Show, Eq)

newtype ContainerID = ContainerID Text deriving (Show, Eq)

instance Aeson.FromJSON ContainerID where
  parseJSON = Aeson.withObject "ContrainerID" $ \obj ->
    ContainerID <$> obj .: "Id"

data ContainerStatus
  = ContainerRunning
  | ContainerExited ContainerExitCode
  | ContainerOther Text
  deriving (Show, Eq)

instance Aeson.FromJSON ContainerStatus where
  parseJSON = Aeson.withObject "ContainerStatus" $ \obj -> do
    state <- obj .: "State"
    status <- state .: "Status"
    case status of
      "exited" -> do
        code <- state .: "ExitCode"
        pure $ ContainerExited (ContainerExitCode code)
      "running" -> pure ContainerRunning
      _ -> pure $ ContainerOther status

imageToText :: Docker.Image -> Text
imageToText (Docker.Image t) = t

containerIdToText :: ContainerID -> Text
containerIdToText (ContainerID i) = i

newtype ContainerExitCode = ContainerExitCode Int deriving (Show, Eq)

data ContainerCreateOptions = ContainerCreateOptions
  { image :: Image,
    script :: Text
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
            ("EntryPoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"]),
            ("Cmd", "echo \"$CI_SCRIPT\" | /bin/sh"),
            ("Env", Aeson.toJSON ["CI_SCRIPT=" <> options.script])
          ]
      request =
        mkReq "containers/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body

  res <- HTTP.httpBS request
  parseResponse res

startContainer_ :: RequestBuilder -> ContainerID -> IO ()
startContainer_ mkReq cid = do
  let path = "containers/" <> containerIdToText cid <> "/start"
      request =
        mkReq path
          & HTTP.setRequestMethod "POST"

  void $ HTTP.httpBS request

containerStatus_ :: RequestBuilder -> ContainerID -> IO ContainerStatus
containerStatus_ mkReq cid = do
  let path = "containers/" <> containerIdToText cid <> "/json"
      request = mkReq path & HTTP.setRequestMethod "GET"

  resp <- HTTP.httpBS request
  parseResponse resp

createVolume_ :: RequestBuilder -> IO Volume
createVolume_ mkReq = do
  let body = Aeson.object [("Labels", Aeson.object [("ci", "")])]
      request =
        mkReq "volumes/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body

  res <- HTTP.httpBS request
  parseResponse res

parseResponse :: (Aeson.FromJSON a) => HTTP.Response ByteString -> IO a
parseResponse res = do
  case Aeson.eitherDecodeStrict (HTTP.getResponseBody res) of
    Left e -> throwString e
    Right status -> pure status
