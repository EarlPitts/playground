module TypelevelStuff.PhantomTypes where

data Validated
data NotValidated
newtype Email a = Email String deriving Show -- This has to be opaque

mkEmail :: String -> Email NotValidated
mkEmail = Email

validate :: Email NotValidated -> Maybe (Email Validated)
validate (Email e) = if elem '@' e then Just (Email e) else Nothing

sendEmail :: Email Validated -> IO ()
sendEmail (Email e) = putStrLn ("Email sent to " ++ e)

-- Only allow checking email for authed user
data Authenticated
data Unauthenticated

data User isAuthenticated = User
  { userName :: String
  , userEmail :: String
  , userPassword :: String
  }
  deriving (Show, Eq)

getUserName :: User isAuthenticated -> String
getUserName = userName

getUserEmail :: User Authenticated -> String
getUserEmail = userEmail

mkUser :: String -> String -> String -> User Unauthenticated
mkUser = User

authenticate :: User Unauthenticated -> String -> Maybe (User Authenticated)
authenticate User{..} pass
  | userPassword == pass = Just User{..}
  | otherwise = Nothing

user = mkUser "jani" "jani@jani.com" "jani123"
