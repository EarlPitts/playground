-- https://blog.ploeh.dk/2019/12/02/refactoring-registration-flow-to-functional-architecture/

module Architecture.RegistrationFlowIO where

import Data.Bitraversable
import Data.IORef
import qualified Data.Map as Map
import RIO ((&), (<&>))
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
          Just (_, True) -> pure True
          _ -> pure False
    }

fakeRegistrationService :: IORef [Registration] -> RegistrationService
fakeRegistrationService registrations = do
  RegistrationService
    { completeRegistration = \reg -> modifyIORef registrations (\rs -> (reg : rs))
    }

completeRegistrationWorkflow ::
  Registration ->
  Maybe Bool ->
  Either Mobile Registration
completeRegistrationWorkflow registration proof =
  case proof of
    Just True -> Right registration
    _ -> Left registration.mobile

fixture = do
  proofs <- newIORef Map.empty
  registrations <- newIORef []
  let proofService = fakeProofService proofs
  let regService = fakeRegistrationService registrations
  -- let sut pid r = do
  --       b <- traverse (proofService.verifyProof r.mobile) pid
  --       let res = completeRegistrationWorkflow r b
  --       p <- bitraverse proofService.createProof regService.completeRegistration res
  --       pure $ either ProofRequired (const RegistrationCompleted) p
  -- let sut pid r =
  --       traverse (proofService.verifyProof r.mobile) pid
  --         <&> completeRegistrationWorkflow r
  --         >>= bitraverse proofService.createProof regService.completeRegistration
  --           <&> either ProofRequired (const RegistrationCompleted)
  let sut pid r = do
        validity <- traverse (proofService.verifyProof r.mobile) pid -- impure
        let decision = completeRegistrationWorkflow r validity -- pure
        decision -- impure
          & (bitraverse proofService.createProof regService.completeRegistration)
          <&> (either ProofRequired (const RegistrationCompleted))
  pure (sut, proofs, registrations)

tests = hspec $ do
  it "" $ do
    (sut, proofs, registrations) <- fixture
    modifyIORef proofs (\ps -> Map.insert (Mobile 234) (123, True) ps)
    result <- sut (Just 123) (Registration (Mobile 234))
    rs <- readIORef registrations
    result `shouldBe` RegistrationCompleted
    rs `shouldBe` [Registration (Mobile 234)]
  it "" $ do
    (sut, _, registrations) <- fixture
    result <- sut Nothing (Registration (Mobile 234))
    rs <- readIORef registrations
    result `shouldSatisfy` \case
      ProofRequired _ -> True
      _ -> False
    rs `shouldBe` []
  it "" $ do
    (sut, proofs, registrations) <- fixture
    modifyIORef proofs (\ps -> Map.insert (Mobile 234) (123, False) ps)
    result <- sut (Just 123) (Registration (Mobile 234))
    rs <- readIORef registrations
    result `shouldBe` ProofRequired 123
    rs `shouldBe` []
