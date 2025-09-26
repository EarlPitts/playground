{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module MonadThrow where

import Control.Monad
import Control.Monad.Catch

data User = User Int Int String String deriving (Show, Eq)

data UserErr
  = NotFound
  | AlreadyInDB
  | InvalidAge
  | InvalidName
  deriving (Show, Eq)

instance Exception UserErr

newtype UserRepository m = UserRepository
  {add :: Int -> String -> String -> m Int}

mkUserRepoService :: (MonadThrow m) => UserRepository m
mkUserRepoService =
  UserRepository
    { add = \_ name _ ->
        if name == "John"
          then throwM AlreadyInDB
          else pure 1
    }

newtype UserService m = UserService
  {createUser :: Int -> String -> String -> m User}

mkUserService :: (MonadThrow m, MonadCatch m) => UserRepository m -> UserService m
mkUserService repo =
  UserService
    { createUser = \age name address -> do
        validateAge age
        validateName name
        uid <- add repo age name address
        return $ User uid age name address
    }

validateAge :: (MonadThrow m) => Int -> m ()
validateAge age =
  when (age > 100 || age < 0) (throwM InvalidAge)

validateName :: (MonadThrow m) => String -> m ()
validateName name =
  when (length name > 10) (throwM InvalidName)

service :: UserService IO
service = mkUserService mkUserRepoService

handler :: UserErr -> IO ()
handler = print

mkUser :: UserService m -> Int -> String -> String -> m User
mkUser = createUser

mkUser' :: UserService IO -> Int -> String -> String -> IO ()
mkUser' userService age name address = catch (createUser userService age name address >>= print) handler
