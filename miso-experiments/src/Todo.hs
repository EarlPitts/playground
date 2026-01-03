{-# language NamedFieldPuns    #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards   #-}
{-# language ScopedTypeVariables #-}

module Todo where

import Miso hiding ((!!))
import Miso.String (MisoString, ms)
import Miso.Html.Property hiding (label_)
import Miso.Html.Event
import Miso.Html.Element hiding (style_)
import System.Random
import Miso.Lens
import Data.Time.LocalTime
import Control.Monad.IO.Class

todoApp :: App Model Action
todoApp = (component initialItems updateModel viewModel)
  {
    view          = viewModel,
    events        = defaultEvents,
    subs          = [],
    mountPoint    = Nothing,
    logLevel      = Off
  }

data ListItem
  = ListItem {
      liId   :: MisoString
    , liText :: MisoString
    , liDone :: Bool
    }
  deriving (Show, Eq)

data Model = Model { _todos :: [ListItem] } deriving (Show, Eq)

todosL :: Lens Model [ListItem]
todosL = lens _todos $ \m v -> m { _todos = v }

initialItems :: Model
initialItems = Model {
  _todos = [ ListItem "lunch"    "Have lunch"           True
          , ListItem "workshop" "Give a Miso workshop" False
          ]
  }

type Id = MisoString

data Action
  = None
  | ToggleDone Id
  | RandomTodo
  | AddTodo ListItem
  deriving (Show, Eq)

updateModel :: Action -> Effect ROOT Model Action
updateModel (ToggleDone listId) =
  todosL %= fmap (\li -> if (liId li) == listId then li { liDone = not (liDone li)} else li)
updateModel RandomTodo = io $ do
  txt <- (["eat", "drink", "sleep"] !!) <$> randomRIO (0,2)
  time <- liftIO $ getZonedTime
  return $ AddTodo (ListItem (ms (show time)) txt False)
updateModel (AddTodo li) = todosL %= flip (++) [li]
updateModel _ = pure ()

bootstrapUrl :: MisoString
bootstrapUrl = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"

viewModel :: Model -> View Model Action
viewModel m
  = div_ [ class_ "container"]
         [ header
         , ul_ [ class_ "list-group" ] (map viewListItem (_todos m))
         , link_ [ rel_ "stylesheet"
                 , href_ bootstrapUrl ] ]

viewListItem :: ListItem -> View Model Action
viewListItem ListItem { .. }
  = li_ [ class_ "list-group-item"]
        [ div_  [ class_ "custom-control custom-checkbox"]
                [ input_ [ class_   "custom-control-input"
                         , type_    "checkbox"
                         , value_   "on"
                         , checked_ liDone
                         , id_      liId
                         , onChecked (const $ ToggleDone liId)
                         ]
                , label_ [ class_ (if liDone
                                      then "custom-control-label text-muted"
                                      else "custom-control-label")
                         , for_   liId ]
                         [ text liText ]] ]

header :: View Model Action
header
  = nav_ [ class_ "navbar navbar-dark bg-dark"]
         [ h2_ [ class_ "bd-title text-light" ]
               [ text "To-do "
               , span_ [ class_ "badge badge-warning"]
                       [ text "in miso!"] ]
         , -- form [ class_ "form-inline" ]
                 button_ [ class_ "btn btn-outline-warning"
                          , type_ "button"
                          , onClick RandomTodo ]
                [ text "New random to-do" ] 
         ]
