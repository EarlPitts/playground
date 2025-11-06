module Docker where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock.POSIX as Time
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified RIO.Text as Text
import qualified RIO.Text.Partial as Text.Partial
import qualified Socket

type RequestBuilder = Text -> HTTP.Request

data Service = Service
  { createContainer :: ContainerCreateOptions -> IO ContainerID,
    startContainer :: ContainerID -> IO (),
    containerStatus :: ContainerID -> IO ContainerStatus,
    createVolume :: IO Volume,
    fetchLogs :: FetchLogsOptions -> IO ByteString,
    pullImage :: Image -> IO ()
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
        createVolume = createVolume_ mkReq,
        fetchLogs = fetchLogs_ mkReq,
        pullImage = pullImage_ mkReq
      }

newtype Volume = Volume Text deriving (Show, Eq)

instance Aeson.FromJSON Volume where
  parseJSON = Aeson.withObject "CreateVolume" $ \obj ->
    Volume <$> obj .: "Name"

data Image = Image
  { name :: Text,
    tag :: Text
  }
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON Image where
  parseJSON = Aeson.withText "Image" $ \str ->
    case Text.Partial.splitOn ":" str of
      [name, tag] -> pure $ Image {name = name, tag = tag}
      [name] -> pure $ Image {name = name, tag = "latest"}
      _ -> fail $ "Image has too many colons " <> Text.unpack str

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
imageToText (Docker.Image name tag) = name <> ":" <> tag

volumeToText :: Docker.Volume -> Text
volumeToText (Docker.Volume t) = t

containerIdToText :: ContainerID -> Text
containerIdToText (ContainerID i) = i

newtype ContainerExitCode = ContainerExitCode Int deriving (Show, Eq)

data ContainerCreateOptions = ContainerCreateOptions
  { image :: Image,
    script :: Text,
    volume :: Volume
  }
  deriving (Show, Eq)

data FetchLogsOptions = FetchLogsOptions
  { container :: ContainerID,
    from :: Time.POSIXTime,
    to :: Time.POSIXTime
  }
  deriving (Show, Eq)

createContainer_ :: RequestBuilder -> ContainerCreateOptions -> IO ContainerID
createContainer_ mkReq options = do
  let image = imageToText options.image
      bind = volumeToText options.volume <> ":/app"
      body =
        Aeson.object
          [ ("Image", Aeson.toJSON image),
            ("Tty", Aeson.toJSON True),
            ("Labels", Aeson.object [("ci", "")]),
            ("EntryPoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"]),
            ("Cmd", "echo \"$CI_SCRIPT\" | /bin/sh"),
            ("HostConfig", Aeson.object [("Binds", Aeson.toJSON [bind])]),
            ("WorkingDir", "/app"),
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
      request = mkReq path

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

fetchLogs_ :: RequestBuilder -> FetchLogsOptions -> IO ByteString
fetchLogs_ mkReq options = do
  let timeStampToText t = tshow (round t :: Int)
  let path =
        "containers/"
          <> containerIdToText options.container
          <> "/logs?stdout=true&stderr=true&since="
          <> timeStampToText options.from
          <> "&until="
          <> timeStampToText options.to

  resp <- HTTP.httpBS $ mkReq path
  pure $ HTTP.getResponseBody resp

pullImage_ :: RequestBuilder -> Image -> IO ()
pullImage_ mkReq image = do
  let path =
        "/images/create?tag="
          <> image.tag
          <> "&fromImage="
          <> image.name

      request =
        mkReq path
          & HTTP.setRequestMethod "POST"

  void $ HTTP.httpBS request

parseResponse :: (Aeson.FromJSON a) => HTTP.Response ByteString -> IO a
parseResponse res = do
  case Aeson.eitherDecodeStrict (HTTP.getResponseBody res) of
    Left e -> throwString e
    Right status -> pure status
