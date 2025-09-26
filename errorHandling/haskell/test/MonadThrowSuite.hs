{-# LANGUAGE FlexibleInstances #-}

module MonadThrowSuite where

import Control.Exception
import Data.Functor.Identity
import Control.Monad.State
import MonadThrow
import Test.Hspec

validAge = 30

validName = "name"

validAddress = "address"

invalidAge = 120

invalidName = "verylongname"

existingName = "John"

stubService :: UserService Identity
stubService =
  UserService
    { createUser = \age name address -> return (User 1 age name address)
    }

stubRepo :: Int -> UserRepository (StateT [String] (Either SomeException))
stubRepo resp =
  UserRepository
    { add = \_ _ _ -> modify ("add called" :) >> return resp
    }

tests :: IO ()
tests = hspec $ do
  describe "mkUser" $ do
    it "creates a user if everything is valid" $ do
      runIdentity (mkUser stubService validAge validName validAddress) `shouldBe` User 1 validAge validName validAddress

  describe "stubService" $ do
    it "creates a user if everything is valid" $ do
      let result = runStateT (createUser (mkUserService $ stubRepo 1) validAge validName validAddress) []
      case result of
        Right s -> s `shouldBe` (User 1 validAge validName validAddress, ["add called"])
        Left e -> expectationFailure ("Unexpected error: " ++ show e)

    it "creates a user if everything is valid" $ do
      runIdentity (mkUser stubService validAge validName validAddress) `shouldBe` User 1 validAge validName validAddress

-- it "throws an exception if used with an empty list" $ do
--   evaluate (head []) `shouldThrow` anyException
