-- https://blog.ploeh.dk/2019/12/02/refactoring-registration-flow-to-functional-architecture/

module Architecture.RegistrationFlowIO where

import Data.IORef
import qualified Data.Map as Map
import System.Random (randomIO)
import Test.Hspec

data Registration = Registration {mobile :: Mobile} deriving (Show, Eq)

data Mobile = Mobile Int deriving (Eq, Ord, Show)

type ProofId = Int

data CompleteRegistrationResult = ProofRequired ProofId | RegistrationCompleted deriving (Show, Eq)

data ProofService = ProofService
  { createProof :: Mobile -> IO ProofId,
    verifyProof :: Mobile -> ProofId -> IO Bool
  }

data RegistrationService = RegistrationService
  { completeRegistration :: Registration -> IO ()
  }

stubProofService :: ProofService
stubProofService =
  ProofService
    { createProof = \_ -> pure 1,
      verifyProof = \_ pId -> pure $ pId == 1
    }

stubRegistrationService :: RegistrationService
stubRegistrationService =
  RegistrationService
    { completeRegistration = \_ -> pure ()
    }

fakeProofService :: IORef (Map.Map Mobile (Int, Bool)) -> ProofService
fakeProofService proofs =
  ProofService
    { createProof = \m -> do
        map <- readIORef proofs
        case Map.lookup m map of
          Just (proofId, _) -> pure proofId
          Nothing -> do
            proofId <- randomIO :: IO Int
            modifyIORef proofs (\map -> Map.insert m (proofId, False) map)
            pure proofId,
      verifyProof = \m _ -> do
        map <- readIORef proofs
        case Map.lookup m map of
          Nothing -> pure False
          Just (_, True) -> pure True
    }

fakeRegistrationService :: IORef [Registration] -> RegistrationService
fakeRegistrationService registrations = do
  RegistrationService
    { completeRegistration = \reg -> modifyIORef registrations (\rs -> (reg : rs))
    }

completeRegistrationWorkflow ::
  ProofService ->
  RegistrationService ->
  Maybe ProofId ->
  Registration ->
  IO CompleteRegistrationResult
completeRegistrationWorkflow proofService registrationService proofId registration =
  case proofId of
    Nothing ->
      ProofRequired <$> proofService.createProof registration.mobile
    Just pId -> do
      isValid <- proofService.verifyProof registration.mobile pId
      if isValid
        then do
          registrationService.completeRegistration registration
          pure RegistrationCompleted
        else
          ProofRequired <$> proofService.createProof registration.mobile

fixture = do
  proofs <- newIORef Map.empty
  registrations <- newIORef []
  let proofService = fakeProofService proofs
  let regService = fakeRegistrationService registrations
  let sut = completeRegistrationWorkflow proofService regService
  pure (sut, proofs, registrations)

tests = hspec $ do
  it "" $ do
    (sut, proofs, registrations) <- fixture
    modifyIORef proofs (\ps -> Map.insert (Mobile 234) (123, True) ps)
    result <- sut (Just 123) (Registration (Mobile 234))
    rs <- readIORef registrations
    result `shouldBe` RegistrationCompleted
    rs `shouldBe` [Registration (Mobile 234)]
