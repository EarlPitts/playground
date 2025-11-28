module Architecture.RegistrationFlowState where

import Control.Monad.State
import qualified Data.Map as Map
import Test.Hspec

data Registration = Registration {mobile :: Mobile} deriving (Show, Eq)

data Mobile = Mobile Int deriving (Eq, Ord, Show)

type ProofId = Int

data CompleteRegistrationResult = ProofRequired ProofId | RegistrationCompleted deriving (Show, Eq)

data ProofService m = ProofService
  { createProof :: Mobile -> m ProofId,
    verifyProof :: Mobile -> ProofId -> m Bool
  }

data RegistrationService m = RegistrationService
  { completeRegistration :: Registration -> m ()
  }

stubProofService :: ProofService IO
stubProofService =
  ProofService
    { createProof = \_ -> pure 1,
      verifyProof = \_ pId -> pure $ pId == 1
    }

stubRegistrationService :: RegistrationService IO
stubRegistrationService =
  RegistrationService
    { completeRegistration = \_ -> pure ()
    }

fakeProofService :: ProofService (State ([Registration], (Map.Map Mobile (Int, Bool))))
fakeProofService = do
  ProofService
    { createProof = \m -> do
        (_, proofs) <- get
        case Map.lookup m proofs of
          Just (proofId, _) -> pure proofId
          Nothing -> do
            let proofId = 3
            modify (\(rs, map) -> (rs, Map.insert m (proofId, False) map))
            pure proofId,
      verifyProof = \m _ -> do
        (_, proofs) <- get
        case Map.lookup m proofs of
          Nothing -> pure False
          Just (_, True) -> pure True
          _ -> pure False
    }

fakeRegistrationService :: RegistrationService (State ([Registration], (Map.Map Mobile (Int, Bool))))
fakeRegistrationService = do
  RegistrationService
    { completeRegistration = \reg -> modify (\(rs, ps) -> ((reg : rs), ps))
    }

completeRegistrationWorkflow ::
  (Monad m) =>
  ProofService m ->
  RegistrationService m ->
  Maybe ProofId ->
  Registration ->
  m CompleteRegistrationResult
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

tests = hspec $ do
  it "" $ do
    let registrations = []
    let proofs = Map.fromList [(Mobile 234, (123, True))]
    let sut = completeRegistrationWorkflow fakeProofService fakeRegistrationService

    let (res, (rs, _)) = runState (sut (Just 123) (Registration (Mobile 234))) (registrations, proofs)

    res `shouldBe` RegistrationCompleted
    rs `shouldBe` [Registration (Mobile 234)]
  it "" $ do
    let registrations = []
    let proofs = Map.fromList [(Mobile 234, (123, True))]
    let sut = completeRegistrationWorkflow fakeProofService fakeRegistrationService

    let (res, (rs, _)) = runState (sut Nothing (Registration (Mobile 234))) (registrations, proofs)

    res `shouldSatisfy` \case
      ProofRequired _ -> True
      _ -> False
    rs `shouldBe` []
  it "" $ do
    let registrations = []
    let proofs = Map.fromList [(Mobile 234, (123, False))]
    let sut = completeRegistrationWorkflow fakeProofService fakeRegistrationService

    let (res, (rs, _)) = runState (sut (Just 123) (Registration (Mobile 234))) (registrations, proofs)

    res `shouldBe` ProofRequired 123
    rs `shouldBe` []
