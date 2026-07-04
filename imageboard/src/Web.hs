{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web (
  Handle (..),
  Config (..),
  withHandle,
  run
) where

import Control.Applicative (empty, (<|>))
import Control.Monad (void, when)
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NL
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Database (Post (..), Thread (..))
import qualified Database
import qualified Logger
import Lucid
import Network.HTTP.Types.Status (status404)
import Network.Wai.Parse (fileContent)
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
    [(_, pImage)] <- Scotty.files -- TODO
    tSubject <- Scotty.formParam "subject"
    tId <- liftIO $ Database.createThread (hDatabase h) (Database.CreateThread tSubject)
    liftIO $ Logger.logInfo (hLogger h) $ "Created new thread with id " <> show tId
    pId <- liftIO $ Database.createPost (hDatabase h) (Database.CreatePost pText tId True)
    liftIO $ LBS.writeFile ("uploads/" <> show pId) (fileContent pImage)
    Scotty.redirect "/"

  Scotty.post "/newPost/:tId" $ do
    tId <- Scotty.pathParam "tId"
    pText <- Scotty.formParam "text"
    [(_, pImage)] <- Scotty.files
    case fileContent pImage of
      "" -> do
        void $ liftIO $ Database.createPost (hDatabase h) (Database.CreatePost pText tId False)
        Scotty.redirect $ TL.pack ("/thread/" <> show tId)
      image -> do
        pId <- liftIO $ Database.createPost (hDatabase h) (Database.CreatePost pText tId True)
        liftIO $ LBS.writeFile ("uploads/" <> show pId) image
        Scotty.redirect $ TL.pack ("/thread/" <> show tId)

  Scotty.get "/assets/style.css" $ do
    Scotty.setHeader "Content-Type" "text/css"
    Scotty.file "assets/style.css"

  Scotty.get "/uploads/:id" $ do
    fileId <- Scotty.pathParam "id"
    Scotty.setHeader "Content-Type" "image/jpeg"
    Scotty.file $ "uploads/" <> fileId

template :: T.Text -> Html () -> Html ()
template title body = doctypehtml_ $ do
  head_ $ do
    title_ $ toHtml title
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/assets/style.css"]
  body_ $ do
    header_ $ a_ [href_ "/"] "imageboard"
    body

index :: [Thread] -> Html ()
index threads = template "index" $ do
  p_ "Create new thread:"
  form_ [method_ "post", enctype_ "multipart/form-data", action_ "/newThread"] $ do
    -- input_ [name_ "name", placeholder_ "Anonymous", type_ "text"]
    div_ $ input_ [name_ "subject", placeholder_ "Subject", type_ "text"]
    div_ $ textarea_ [name_ "text"] ""
    div_ $ input_ [name_ "image", type_ "file"]
    div_ $ button_ "Post"
  hr_ []
  traverse_ (\t -> threadPreview t >> hr_ []) threads

threadPreview :: Thread -> Html ()
threadPreview Thread{..} = div_ [class_ "posts"] $ do
  img_ [src_ $ "/uploads/" <> T.pack (show (pId op))]
  div_ $ do
    span_ $ toHtml tSubject
    " "
    span_ $ toHtml (show $ pCreated op)
    " "
    span_ $ a_ [href_ $ T.pack $ "thread/" <> show tId] "Reply"
    p_ $ toHtml (pText op)
 where
  op = NL.head tPosts

threadView :: Thread -> Html ()
threadView Thread{..} = template tSubject $ do
  p_ "Create new post:"
  form_ [method_ "post", enctype_ "multipart/form-data", action_ $ T.pack $ "/newPost/" <> show tId] $ do
    div_ $ textarea_ [name_ "text"] ""
    div_ $ input_ [name_ "image", type_ "file"]
    div_ $ button_ "Post"
  hr_ []
  toHtml tSubject
  traverse_ (\p -> postView p >> hr_ []) tPosts

postView :: Post -> Html ()
postView Post{..} = div_ [class_ "posts"] $ do
  when pWithImage (span_ $ img_ [src_ $ "/uploads/" <> T.pack (show pId)])
  div_ $ do
    span_ $ toHtml (show pCreated)
    " "
    span_ $ toHtml (show pId)
    p_ $ toHtml pText
