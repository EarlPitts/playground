{-# LANGUAGE FlexibleContexts #-}

module UserService where

import Control.Concurrent.STM
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.List
import Network.Socket
import Types

data User = User
  { userName :: UserName,
    nickName :: NickName,
    host :: Host,
    serverName :: ServerName,
    realName :: RealName,
    userSocket :: Socket
  }
  deriving (Show, Eq)

data UserServiceError
  = ExistingUser
  | UserNotFound
  deriving (Show, Eq)

data UserStorage = UserStorage
  { loadUsers :: STM [User],
    storeUsers :: [User] -> STM ()
  }

data UserService = UserService
  { addUser :: User -> ExceptT UserServiceError IO User,
    removeUser :: NickName -> IO (),
    findUser :: NickName -> MaybeT IO User,
    getUsers :: IO [User]
  }

mkUserService :: UserStorage -> UserService
mkUserService storage =
  UserService
    { addUser = \user -> do
        ExceptT $ liftIO $ atomically $ do
          users <- loadUsers storage
          if user `elem` users
            then return $ Left ExistingUser
            else do
              storeUsers storage (user : users)
              return $ Right user,
      removeUser = \nick -> liftIO $ atomically $ do
        users <- loadUsers storage
        case find (\u -> nickName u == nick) users of
          Nothing -> return ()
          Just u -> storeUsers storage (delete u users),
      findUser = \nick -> MaybeT $ liftIO $ atomically $ do
        users <- loadUsers storage
        return $ find (\u -> nickName u == nick) users,
      getUsers = atomically $ loadUsers storage
    }

mkUserStorage :: TVar [User] -> UserStorage
mkUserStorage store =
  UserStorage
    { loadUsers = readTVar store,
      storeUsers = writeTVar store
    }
