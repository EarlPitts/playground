module DTO where

import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import qualified Data.Bifunctor as BF
import Data.ByteString.Lazy (ByteString)
import Data.Either.Validation (Validation (Failure, Success), validationToEither)
import Data.List (singleton)
import Data.Time
import GHC.Generics

newtype BirthDate = BirthDate {date :: Day} deriving (Show)
newtype String50 = String50 {unString :: String} deriving (Show)

data Person = Person
  { firstName :: String50
  , lastName :: String50
  , birthdate :: BirthDate
  }
  deriving (Show)

data PersonDTO = PersonDTO
  { firstNameDto :: String
  , lastNameDto :: String
  , birthdateDto :: Day
  }
  deriving (Show, Generic, FromJSON, ToJSON)

mkString50' :: String -> Validation [String] String50
mkString50' str =
  if length str > 50
    then Failure ["too long"]
    else Success $ String50 str

mkBirthDate :: Day -> Validation [String] BirthDate
mkBirthDate day =
  if day < fromGregorian 1900 1 1
    then Failure ["too old"]
    else Success $ BirthDate day

toDomain :: PersonDTO -> Validation [String] Person
toDomain personDTO =
  Person
    <$> mkString50' personDTO.firstNameDto
    <*> mkString50' personDTO.lastNameDto
    <*> mkBirthDate personDTO.birthdateDto

fromDomain :: Person -> PersonDTO
fromDomain (Person firstName lastName date) =
  PersonDTO firstName.unString lastName.unString date.date

birth :: Day
birth = fromGregorian 1892 4 8

examplePerson = PersonDTO (replicate 51 'a') "John" birth

data DtoError
  = ValidationError String
  | DeserializationError String
  deriving (Show)

jsonToDomain :: ByteString -> Either [DtoError] Person
jsonToDomain str = do
  dto <- BF.first (singleton . DeserializationError) $ eitherDecode str
  validationToEither $ BF.first (fmap ValidationError) (toDomain dto)
