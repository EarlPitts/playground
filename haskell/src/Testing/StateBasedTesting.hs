module Testing.StateBasedTesting where

import Control.Monad.State
import Data.List as L
import Data.Map as M
import Test.Hspec

data User = User {id :: Id, nickName :: Nick}

type Id = Int

type Nick = String

data UserService m = UserService
  { saveUser :: User -> m ()
  , loadUser :: Id -> m (Maybe User)
  , loadByNick :: Nick -> m (Maybe User)
  }

register :: (Monad m) => UserService m -> Nick -> m Id
register userService nick = do
  u <- userService.loadByNick nick
  case u of
    Just user -> pure user.id
    Nothing -> userService.saveUser (User 1 nick) >> pure 1

type DB = State (Map Id User)

fakeUserService :: UserService DB
fakeUserService =
  UserService
    { saveUser = \user -> get >>= (\m -> modify (M.insert ((length $ keys m) + 1) user))
    , loadUser = \id -> gets $ M.lookup id
    , loadByNick = \nick -> gets $ (\m -> fmap snd $ find (\(_, u) -> u.nickName == nick) (M.toList m))
    }

main = hspec $ do
  it "New user successfully registers" $ runStateTest $ do
    let db = M.empty
    put db

    actual <- register fakeUserService "Jozsi"

    newDb <- get

    return $
      (actual `shouldBe` ((M.size db) + 1))
        >> ((any (\(_, u) -> u.nickName == "Jozsi") (M.toList newDb)) `shouldBe` True)

runStateTest :: State (Map k a) b -> b
runStateTest = flip evalState (M.empty)

-- (sut, proofs, registrations) <- fixture
-- modifyIORef proofs (\ps -> Map.insert (Mobile 234) (123, True) ps)
-- result <- sut (Just 123) (Registration (Mobile 234))
-- rs <- readIORef registrations
-- result `shouldBe` RegistrationCompleted
-- rs `shouldBe` [Registration (Mobile 234)]
