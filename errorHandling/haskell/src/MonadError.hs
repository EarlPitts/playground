{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module MonadError where

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

type MonadUserErr = MonadError UserErr

newtype UserRepository m = UserRepository
  {add :: Int -> String -> String -> m Int}

mkUserRepoService :: (MonadUserErr m) => UserRepository m
mkUserRepoService =
  UserRepository
    { add = \_ name _ ->
        if name == "Janos"
          then throwError AlreadyInDB
          else pure 1
    }

newtype UserService m = UserService
  {createUser :: Int -> String -> String -> m User}

mkUserService :: (MonadUserErr m) => UserRepository m -> UserService m
mkUserService repo =
  UserService
    { createUser = \age name address -> do
        validateAge age
        validateName name
        uid <- add repo age name address
        return $ User uid age name address
    }

validateAge :: (MonadUserErr m) => Int -> m ()
validateAge age =
  when (age > 100 || age < 0) (throwError InvalidAge)

validateName :: (MonadUserErr m) => String -> m ()
validateName name =
  when (length name > 10) (throwError InvalidName)

service :: UserService (ExceptT UserErr IO)
service = mkUserService mkUserRepoService

service' :: UserService (Except UserErr)
service' = mkUserService mkUserRepoService

mkUser :: Int -> String -> String -> ExceptT UserErr IO User
mkUser = createUser service

mkUser' :: Int -> String -> String -> Except UserErr User
mkUser' = createUser service'

handler :: UserErr -> ExceptT UserErr IO ()
handler InvalidAge = liftIO $ putStrLn "bajvan"

f :: ExceptT UserErr IO ()
f = catchError (void (mkUser 120 "Name" "Address")) handler

f' :: ExceptT UserErr IO ()
f' = void (mkUser 120 "Name" "Address")

f'' :: Except UserErr ()
f'' = void (mkUser' 90 "Name" "Address")
