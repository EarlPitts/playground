import Adapter.PG.Main
import ClassyPrelude
import Data.Pool (Pool (..), createPool, withResource)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

data AppEnv = AppEnv {pgEnv :: PGEnv}

data PGEnv = PGEnv {pgPool :: Pool Connection}

data User = User
  { name :: Text
  }
  deriving (Show, Generic)

instance FromRow User

instance ToRow User

data UserDB m = UserDB
  { addUser :: User -> m Int64,
    getUser :: Int -> m [User]
  }

getUser' :: Connection -> Int -> IO [User]
getUser' conn uid = query conn qry (Only uid)
  where
    qry = "SELECT * FROM users WHERE user_id = ?"

addUser' :: Connection -> User -> IO Int64
addUser' conn usr = execute conn qry usr
  where
    qry = "INSERT INTO users (name) VALUES (?)"

mkMockUserDB :: AppEnv -> UserDB (IO)
mkMockUserDB ae = do
  UserDB addUser'' getUser''
  where
    addUser'' :: User -> IO Int64
    addUser'' usr = return $ 100

    getUser'' :: Int -> IO [User]
    getUser'' usr = return $ [User "Name"]

mkUserDB :: AppEnv -> UserDB (IO)
mkUserDB ae = do
  UserDB (addUser'') (getUser'')
  where
    pool = pgPool $ pgEnv ae
    runDB a = withResource pool a

    getUser'' :: Int -> IO [User]
    getUser'' uid = runDB (\conn -> getUser' conn uid)

    addUser'' :: User -> IO Int64
    addUser'' usr = runDB (\conn -> addUser' conn usr)
