module JSON where

import Data.Aeson
import Data.Char (toLower)
import GHC.Generics

data Shape
  = Circle {radius :: Double}
  | Rectangle {width :: Double, height :: Double}
  deriving (Generic, Show)

customOptions :: Options
customOptions =
  defaultOptions
    { sumEncoding = TaggedObject "type" "contents" -- change tag field name
    , constructorTagModifier = map toLower -- "Circle" -> "circle"
    , fieldLabelModifier = drop 1 -- strip prefix e.g. "_radius" -> "radius"
    , omitNothingFields = True -- skip Nothing fields
    , unwrapUnaryRecords = True -- unwrap single-field records
    , tagSingleConstructors = False -- don't tag single-constructor types
    , rejectUnknownFields = True -- fail on unknown fields (FromJSON only)
    }

instance ToJSON Shape where
  toJSON = genericToJSON customOptions

instance FromJSON Shape where
  parseJSON = genericParseJSON customOptions
