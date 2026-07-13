{-# LANGUAGE NumericUnderscores #-}

module Architecture.MusicRecommender.Handle where

import Data.Foldable
import Data.Function
import Data.IORef
import Data.List
import qualified Data.Map as M
import Data.Ord
import Debug.Trace
import Test.Hspec
import Test.QuickCheck

data Song = Song
  { id :: Int,
    isVerifiedArtist :: Bool,
    rating :: Int
  }
  deriving (Show, Eq)

data Scrobble = Scrobble
  { song :: Song,
    count :: Int
  }
  deriving (Show, Eq)

data User = User
  { name :: String,
    totalScrobbleCount :: Int
  }
  deriving (Show, Eq)

data SongService = SongService
  { getTopListeners :: Int -> IO [User],
    getTopScrobbles :: String -> IO [Scrobble]
  }

type Songs = M.Map Int Song

type Users = M.Map String (M.Map Int Int)

scrobble :: IORef Songs -> IORef Users -> String -> Song -> Int -> IO ()
scrobble songs users uName song scrobbleCount = do
  modifyIORef users f
  modifyIORef songs g
  where
    f = M.insertWith h uName (M.fromList [(song.id, scrobbleCount)])
    g = M.insert song.id song
    h _ scrobbles = addScrobbles scrobbles song scrobbleCount
    addScrobbles scrobbles song scrobbleCount =
      M.insertWith (\_ cnt -> cnt + scrobbleCount) song.id scrobbleCount scrobbles

fakeSongService :: IO (SongService, String -> Song -> Int -> IO ())
fakeSongService = do
  users <- newIORef M.empty
  songs <- newIORef M.empty

  let service =
        SongService
          { getTopListeners = \songId -> do
              us <- readIORef users
              pure $ _getTopListeners us songId,
            getTopScrobbles = \uName -> do
              us <- readIORef users
              ss <- readIORef songs
              pure $ _getTopScrobbles ss us uName
          }
  let setupScrobble = scrobble songs users
  pure (service, setupScrobble)

_getTopListeners :: Users -> Int -> [User]
_getTopListeners users songId = M.foldrWithKey g [] $ M.filter f users
  where
    f = M.member songId
    g uName scrobbles acc =
      let cnt = sum $ M.filterWithKey (\id _ -> id == songId) scrobbles
       in User {name = uName, totalScrobbleCount = cnt} : acc

_getTopScrobbles :: Songs -> Users -> String -> [Scrobble]
_getTopScrobbles songs users uName = case M.lookup uName users of
  Just scrobbles -> M.foldrWithKey f [] scrobbles
  Nothing -> []
  where
    f songId count acc =
      let song = (M.!) songs songId
       in Scrobble {song = song, count = count} : acc

getRecommendations :: SongService -> String -> IO [Song]
getRecommendations songService userName = do
  scrobbles <- songService.getTopScrobbles userName
  let topScrobbles =
        scrobbles
          & sortOn (Down . count)
          & take 100
  candidates <- fmap concat $ traverse f topScrobbles
  pure $
    candidates
      & sortOn rating
      & take 200
  where
    f scrobble = do
      otherListeners <- songService.getTopListeners scrobble.song.id
      let otherListenersSnapshot =
            otherListeners
              & filter (\u -> u.totalScrobbleCount >= 10000)
              & sortOn (Down . totalScrobbleCount)
              & take 20
      scrobbles <- traverse g otherListenersSnapshot
      pure (fmap song $ concat scrobbles)

    g otherListener = do
      otherScrobbles <- songService.getTopScrobbles (otherListener.name)
      let otherScrobblesSnapshot =
            otherScrobbles
              & filter (\s -> s.song.isVerifiedArtist)
              & sortOn (\s -> s.song.rating)
              & take 10
      pure otherScrobblesSnapshot

alphaNumeric :: Gen Char
alphaNumeric = elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])

userName :: Gen String
userName = do
  length <- choose (1, 19)
  fstLetter <- elements (['a' .. 'z'] ++ ['A' .. 'Z'])
  rest <- vectorOf length alphaNumeric
  return (fstLetter : rest)

instance Arbitrary Song where
  arbitrary = Song <$> arbitrary <*> arbitrary <*> choose (1, 5)

test1 = forAll userName $ \name -> do
  (songService, _) <- fakeSongService
  let sut = getRecommendations songService
  actual <- sut name
  actual `shouldBe` mempty

test2 = do
  (songService, setupScrobble) <- fakeSongService
  setupScrobble "user1" (Song 1 True 5) 100
  setupScrobble "user2" (Song 1 True 3) 50
  let sut = getRecommendations songService

  actual <- sut "user1"

  actual `shouldBe` mempty

test3 = property $ do
  user <- userName
  songs <- (listOf arbitrary)
  cnt <- vectorOf (length songs) (choose (1, 100))
  let scrobbles = zip songs cnt

  return $ do
    (songService, setupScrobble) <- fakeSongService
    traverse_ (\(song, count) -> setupScrobble user song count) scrobbles
    let sut = getRecommendations songService

    actual <- sut user

    actual `shouldBe` mempty

test4 = property $ do
  user <- userName
  songs <- (listOf1 arbitrary)
  cnt <- vectorOf (length songs) (choose (10_000, 14_000))
  let scrobbles = zip songs cnt

  return $ do
    (songService, setupScrobble) <- fakeSongService
    traverse_ (\(song, count) -> setupScrobble user song count) scrobbles
    let sut = getRecommendations songService

    actual <- sut user

    if (all isVerifiedArtist songs)
      then actual `shouldNotBe` mempty
      else pure ()

main :: IO ()
main = hspec $ do
  it "No data" test1
  it "Empty" test2
  it "One user, some songs" test3
  it "One user, some songs with high count" test4
