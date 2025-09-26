{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Either where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.IO.Class

data User = User Int Int String String deriving (Show, Eq)

data UserErr
  = NotFound
  | AlreadyInDB
  | InvalidAge
  | InvalidName
  deriving (Show, Eq)

newtype UserRepository m = UserRepository
  {add :: Int -> String -> String -> m (Either UserErr Int)}

mkUserRepoService :: (Applicative m) => UserRepository m
mkUserRepoService =
  UserRepository
    { add = \_ name _ ->
        if name == "Janos"
          then pure $ Left AlreadyInDB
          else pure $ Right 1
    }

newtype UserService m = UserService
  {createUser :: Int -> String -> String -> m (Either UserErr User)}

mkUserService :: (Applicative m) => UserRepository m -> UserService m
mkUserService repo =
  UserService
    { createUser = \age name address ->
        case liftA2 (,) (validateAge age) (validateName name) of
          Left e -> pure $ Left e
          Right _ ->
            (fmap . fmap) (\uid -> User uid age name address) (add repo age name address)
    }

validateAge :: Int -> Either UserErr ()
validateAge age =
  when (age > 100 || age < 0) (Left InvalidAge)

validateName :: String -> Either UserErr ()
validateName name =
  when (length name > 10) (Left InvalidName)

service :: UserService IO
service = mkUserService mkUserRepoService

-- service' :: UserService (Except UserErr)
-- service' = mkUserService mkUserRepoService

mkUser :: Int -> String -> String -> IO (Either UserErr User)
mkUser = createUser service

-- mkUser' :: Int -> String -> String -> Except UserErr User
-- mkUser' = createUser service'

-- handler :: UserErr -> ExceptT UserErr IO ()
-- handler InvalidAge = liftIO $ putStrLn "bajvan"

f :: IO (Either UserErr User)
f = mkUser 100 "Name" "Address"

-- f' :: ExceptT UserErr IO ()
-- f' = void (mkUser 120 "Name" "Address")
--
-- f'' :: Except UserErr ()
-- f'' = void (mkUser' 90 "Name" "Address")
