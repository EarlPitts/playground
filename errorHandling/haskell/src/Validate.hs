{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Validate where

import Data.List.NonEmpty (NonEmpty, singleton)
import Data.Validation

data User = User Int Int String String deriving (Show, Eq)

data UserErr
  = NotFound
  | AlreadyInDB
  | InvalidAge
  | InvalidName
  deriving (Show, Eq)

newtype UserRepository m = UserRepository
  {add :: Int -> String -> String -> m (Validation (NonEmpty UserErr) Int)}

mkUserRepoService :: (Applicative m) => UserRepository m
mkUserRepoService =
  UserRepository
    { add = \_ name _ ->
        if name == "John"
          then pure $ Failure (singleton AlreadyInDB)
          else pure $ Success 1
    }

newtype UserService m = UserService
  {createUser :: Int -> String -> String -> m (Validation (NonEmpty UserErr) User)}

mkUserService :: (Applicative m) => UserRepository m -> UserService m
mkUserService repo =
  UserService
    { createUser = \age name address ->
        case liftA2 (,) (validateAge age) (validateName name) of
          Failure e -> pure $ Failure e
          Success (validAge, validName) ->
            (fmap . fmap) (\uid -> User uid validAge validName address) (add repo age name address)
    }

validateAge :: Int -> Validation (NonEmpty UserErr) Int
validateAge age =
  if age > 100 || age < 0 then Failure $ singleton InvalidAge else Success age

validateName :: String -> Validation (NonEmpty UserErr) String
validateName name =
  if length name > 10 then Failure $ singleton InvalidName else Success name

service :: UserService IO
service = mkUserService mkUserRepoService

-- service' :: UserService (Except UserErr)
-- service' = mkUserService mkUserRepoService

mkUser :: Int -> String -> String -> IO (Validation (NonEmpty UserErr) User)
mkUser = createUser service

-- mkUser' :: Int -> String -> String -> Except UserErr User
-- mkUser' = createUser service'

-- handler :: UserErr -> ExceptT UserErr IO ()
-- handler InvalidAge = liftIO $ putStrLn "bajvan"

f :: IO (Validation (NonEmpty UserErr) User)
f = mkUser 80 "Janos" "Address"

-- f' :: ExceptT UserErr IO ()
-- f' = void (mkUser 120 "Name" "Address")
--
-- f'' :: Except UserErr ()
-- f'' = void (mkUser' 90 "Name" "Address")
