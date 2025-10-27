module Docker where

import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified Socket

newtype Image = Image Text deriving (Show, Eq)

newtype ContainerID = ContainerID Text deriving (Show, Eq, Generic, Aeson.FromJSON)

data ContainerCreateResponse = ContainerCreateResponse
  { containerId :: ContainerID,
    warning :: [Text]
  }
  deriving (Show, Eq)

instance Aeson.FromJSON ContainerCreateResponse where
  parseJSON = Aeson.withObject "ContrainerCreateResponse" $ \obj ->
    ContainerCreateResponse <$> obj Aeson..: "Id" <*> obj Aeson..: "Warnings"

imageToText :: Docker.Image -> Text
imageToText (Docker.Image t) = t

newtype ContainerExitCode = ContainerExitCode Int deriving (Show, Eq)

data ContainerCreateOptions = ContainerCreateOptions
  { image :: Image
  }
  deriving (Show, Eq)

createContainer :: ContainerCreateOptions -> IO ContainerCreateResponse
createContainer options = do
  manager <- Socket.newManager "/run/docker.sock"

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
  traceShowIO $ res
  let respBody = HTTP.getResponseBody res
  case Aeson.eitherDecodeStrict respBody of
    Left err -> error err
    Right resp -> pure resp
