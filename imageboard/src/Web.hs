{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web where

import Control.Applicative (empty, (<|>))
import Control.Exception.Lifted (Handler (..), catches)
import Control.Monad (forM_, void)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as A
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NL
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Time as Time
import Database (Post (..), Thread (..))
import qualified Database as Database
import qualified Logger as Logger
import Lucid
import Network.HTTP.Types.Status (status404)
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

data Config = Config
  { cPort :: Maybe Int
  , cDomain :: Maybe T.Text
  }
  deriving (Show)

instance Semigroup Config where
  (<>) c1 c2 =
    Config
      { cPort = cPort c1 <|> cPort c2
      , cDomain = cDomain c1 <|> cDomain c2
      }

instance Monoid Config where
  mempty = Config empty empty

instance A.FromJSON Config where
  parseJSON = A.withObject "FromJSON Web.Config" $ \o ->
    Config
      <$> o A..:? "port"
      <*> o A..:? "domain"

data Handle = Handle
  { hConfig :: Config
  , hLogger :: Logger.Handle
  , hDatabase :: Database.Handle
  }

withHandle ::
  Config ->
  Logger.Handle ->
  Database.Handle ->
  (Handle -> IO a) ->
  IO a
withHandle config logger database f =
  f $ Handle config logger database

run :: Handle -> IO ()
run h = Scotty.scotty port (app h)
 where
  port = fromMaybe 8000 $ cPort (hConfig h)

app :: Handle -> ScottyM ()
app h = do
  Scotty.get "/" $ do
    threads <- liftIO $ Database.getThreads (hDatabase h)
    Scotty.html $ renderText (index threads)
  Scotty.get "/thread/:tId" $ do
    tId <- Scotty.pathParam "tId"
    maybeThread <- liftIO $ Database.getThreadById (hDatabase h) tId
    case maybeThread of
      Nothing -> Scotty.status status404
      Just t -> Scotty.html $ renderText (threadView t)
  Scotty.post "/newThread" $ do
    pText <- Scotty.formParam "text"
    tSubject <- Scotty.formParam "subject"
    tId <- liftIO $ Database.createThread (hDatabase h) (Database.CreateThread tSubject)
    void $ liftIO $ Database.createPost (hDatabase h) (Database.CreatePost pText tId)
    Scotty.redirect "/"
  Scotty.post "/newPost/:tId" $ do
    tId <- Scotty.pathParam "tId"
    pText <- Scotty.formParam "text"
    void $ liftIO $ Database.createPost (hDatabase h) (Database.CreatePost pText tId)
    Scotty.redirect $ TL.pack ("/thread/" <> show tId)
  Scotty.get "/assets/style.css" $ do
    Scotty.setHeader "Content-Type" "text/css"
    Scotty.file "assets/style.css"

template :: T.Text -> Html () -> Html ()
template title body = doctypehtml_ $ do
  head_ $ do
    title_ $ toHtml title
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/assets/style.css"] -- TODO
  body_ $ do
    header_ $ a_ [href_ "/"] "imageboard"
    body

-- footer_ "Very nice footer text"

index :: [Thread] -> Html ()
index threads = template "index" $ do
  p_ "Create new thread:"
  form_ [method_ "post", enctype_ "multipart/form-data", action_ "/newThread"] $ do
    -- input_ [name_ "name", placeholder_ "Anonymous", type_ "text"]
    div_ $ input_ [name_ "subject", placeholder_ "Subject", type_ "text"]
    div_ $ textarea_ [name_ "text"] ""
    -- input_ [name_ "image", type_ "file"]
    div_ $ button_ "Post"
  hr_ []
  traverse_ threadPreview threads

threadPreview :: Thread -> Html ()
threadPreview Thread{..} = div_ $ do
  span_ "Thread Id: " <> toHtml (show tId)
  span_ " Subject: " <> toHtml tSubject
  div_ $ toHtml (show $ pCreated op)
  div_ $ a_ [href_ $ T.pack $ "thread/" <> show tId] "Reply"
  p_ $ toHtml (pText op)
  hr_ []
 where
  op = NL.head tPosts

threadView :: Thread -> Html ()
threadView Thread{..} = template tSubject $ do
  p_ "Create new post:"
  form_ [method_ "post", enctype_ "multipart/form-data", action_ $ T.pack $ "/newPost/" <> show tId] $ do
    div_ $ textarea_ [name_ "text"] ""
    div_ $ button_ "Post"
  span_ "Thread Id: " <> toHtml (show tId)
  span_ " Subject: " <> toHtml tSubject
  hr_ []
  traverse_ postView tPosts

postView :: Post -> Html ()
postView Post{..} = div_ $ do
  span_ "Post Id: " <> toHtml (show pId)
  div_ $ toHtml (show pCreated)
  p_ $ toHtml pText
  hr_ []
