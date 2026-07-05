{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.View where

import Control.Monad (when)
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NL
import Data.Text (Text)
import qualified Data.Text as T
import Database (Post (..), Thread (..))
import Lucid

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
    div_ $ input_ [name_ "subject", placeholder_ "Subject", type_ "text", required_ "required"]
    div_ $ textarea_ [name_ "text", required_ "required"] ""
    div_ $ input_ [name_ "image", type_ "file", required_ "required"]
    div_ $ button_ "Post"
  hr_ []
  traverse_ (\t -> threadPreview t >> hr_ []) threads

threadPreview :: Thread -> Html ()
threadPreview Thread{..} = div_ [class_ "posts"] $ do
  let imgPath = "/uploads/" <> T.pack (show (pId op))
  a_ [href_ imgPath] (img_ [src_ imgPath])
  div_ $ do
    span_ $ b_ (toHtml tSubject)
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
    div_ $ textarea_ [name_ "text", required_ "required"] ""
    div_ $ input_ [name_ "image", type_ "file"]
    div_ $ button_ "Post"
  hr_ []
  postView (Just tSubject) op
  hr_ []
  traverse_ (\p -> postView Nothing p >> hr_ []) posts
 where
  op = NL.head tPosts
  posts = NL.tail tPosts

postView :: Maybe Text -> Post -> Html ()
postView subject Post{..} = div_ [class_ "posts"] $ do
  when pWithImage $ do
    let imgPath = "/uploads/" <> T.pack (show pId)
    a_ [href_ imgPath] (img_ [src_ imgPath])
  div_ $ do
    case subject of
      Nothing -> pure ()
      Just s -> span_ $ b_ (toHtml s) <> " "
    span_ $ toHtml (show pCreated)
    " "
    span_ $ toHtml (show pId)
    p_ $ toHtml pText
