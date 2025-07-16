data Validated
data NotValidated
newtype Email a = Email String deriving Show -- This has to be opaque

mkEmail :: String -> Email NotValidated
mkEmail = Email

validate :: Email NotValidated -> Maybe (Email Validated)
validate (Email e) = if elem '@' e then Just (Email e) else Nothing

sendEmail :: Email Validated -> IO ()
sendEmail (Email e) = putStrLn ("Email sent to " ++ e)
