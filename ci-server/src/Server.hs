module Server where

import qualified Codec.Serialise as Serialise
import Core
import qualified Data.Aeson as Aeson
import qualified GitHub as GH
import qualified JobHandler
import RIO
import qualified RIO.NonEmpty as NonEmpty
import qualified Web.Scotty as Scotty

data Config = Config
  { port :: Int
  }
  deriving (Show, Eq)

run :: Config -> JobHandler.Service -> IO ()
run config handler = Scotty.scotty config.port do
  Scotty.post "/agent/pull" do
    cmd <- Scotty.liftAndCatchIO handler.dispatchCmd
    Scotty.raw $ Serialise.serialise cmd

  Scotty.post "/agent/send" do
    msg <- Serialise.deserialise <$> Scotty.body
    Scotty.liftAndCatchIO $ handler.processMsg msg
    Scotty.json $ ("Message Processed" :: Text)

  Scotty.post "/webhook/github" do
    body <- Scotty.body

    number <- Scotty.liftAndCatchIO do
      info <- GH.parsePushEvent (toStrictBytes body)
      pipeline <- GH.fetchRemotePipeline info

      let step = GH.createCloneStep info
      handler.queueJob
        $ pipeline
          { steps = NonEmpty.cons step pipeline.steps
          }

    Scotty.json
      $ Aeson.object
        [ ("number", Aeson.toJSON $ buildNumberToInt number),
          ("status", "job queued")
        ]
