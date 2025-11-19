module GitHub where

import Core
import qualified Data.Aeson as Aeson
import Data.Yaml as Yaml
import qualified Docker as Docker
import qualified JobHandler
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial

parsePushEvent :: ByteString -> IO JobHandler.CommitInfo
parsePushEvent body =
  case Aeson.eitherDecodeStrict body of
    Left e -> throwString e
    Right info -> pure info

fetchRemotePipeline :: JobHandler.CommitInfo -> IO Pipeline
fetchRemotePipeline info = do
  endpoint <- HTTP.parseRequest "https://api.github.com/"
  let path = "repos/" <> info.repo <> "/contents/.quad.yml"

  res <-
    HTTP.httpBS
      $ endpoint
      & HTTP.setRequestPath (encodeUtf8 path)
      & HTTP.addToRequestQueryString [("ref", Just $ encodeUtf8 info.sha)]
      & HTTP.addRequestHeader "User-Agent" "quad-ci"
      & HTTP.addRequestHeader "Accept" "application/vnd.github.v3.raw"

  Yaml.decodeThrow (HTTP.getResponseBody res)

createCloneStep :: JobHandler.CommitInfo -> Step
createCloneStep info =
  Step
    { name = StepName "clone",
      commands =
        NonEmpty.Partial.fromList
          [ "git clone -q https://github.com/" <> info.repo <> " .",
            "git checkout -qf " <> info.sha
          ],
      image = Docker.Image "alpine/git" "v2.49.1"
    }
