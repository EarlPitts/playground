{-# LANGUAGE FlexibleInstances #-}

module ValidateSuite where

import Control.Exception
import Control.Monad.Except
import Control.Monad.State
import Data.Functor.Identity
import Data.List.NonEmpty
import Data.Validation
import Test.Hspec
import Validate

validAge = 30

validName = "name"

validAddress = "address"

invalidAge = 120

invalidName = "verylongname"

existingName = "John"

-- stubService :: UserService Identity
-- stubService =
--   UserService
--     { createUser = \age name address -> return (User 1 age name address)
--     }

stubRepo :: Int -> UserRepository (State [String])
stubRepo resp =
  UserRepository
    { add = \_ name _ -> do
        put ["add called"]
        if name == existingName
          then return (Failure (singleton AlreadyInDB))
          else return (Success resp)
    }

tests :: IO ()
tests = hspec $ do
  -- describe "mkUser" $ do
  --   it "creates a user if everything is valid" $ do
  --     runIdentity (mkUser stubService validAge validName validAddress) `shouldBe` User 1 validAge validName validAddress

  describe "stubService" $ do
    it "creates a user if everything is valid" $ do
      let result = runState (createUser (mkUserService $ stubRepo 1) validAge validName validAddress) []
      result `shouldBe` (Success $ User 1 validAge validName validAddress, ["add called"])

    it "invalid age" $ do
      let result = runState (createUser (mkUserService $ stubRepo 1) invalidAge validName validAddress) []
      result `shouldBe` (Failure (singleton InvalidAge), [])

    it "invalid age, invalid name" $ do
      let result = runState (createUser (mkUserService $ stubRepo 1) invalidAge invalidName validAddress) []
      result `shouldBe` (Failure $ fromList [InvalidAge, InvalidName], [])

    it "already in db" $ do
      let result = runState (createUser (mkUserService $ stubRepo 1) validAge existingName validAddress) []
      result `shouldBe` (Failure $ fromList [AlreadyInDB], ["add called"])

-- case result of
--   Right s -> s `shouldBe` (User 1 validAge validName validAddress, ["add called"])
--   Left e -> expectationFailure ("Unexpected error: " ++ show e)

-- it "age is invalid" $ do
--   let result = runStateT (createUser (mkUserService $ stubRepo 1) invalidAge validName validAddress) []
--   case result of
--     Right s -> s `shouldBe` (User 1 validAge validName validAddress, ["add called"])
--     Left e -> e `shouldThrow` InvalidAge
--
-- it "creates a user if everything is valid" $ do
--   runIdentity (mkUser stubService validAge validName validAddress) `shouldBe` User 1 validAge validName validAddress

-- it "throws an exception if used with an empty list" $ do
--   evaluate (head []) `shouldThrow` anyException
