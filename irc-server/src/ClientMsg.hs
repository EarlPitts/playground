module ClientMsg where

import Text.Parsec
import Text.Parsec.String
import Types
import Data.ByteString
import qualified Data.ByteString.Char8 as C8

data ClientMsg
  = Unimplemented
  | Nick NickName
  | NewUser UserName Host ServerName RealName
  | Ping Host
  | Quit
  deriving (Show, Eq)

-- pMsgs :: Parser [ClientMsg]
-- pMsgs = pClientMsg `sepEndBy` crlf

pClientMsg :: Parser ClientMsg
pClientMsg = pUser <|> pNick <|> pPing <|> pQuit

pPing :: Parser ClientMsg
pPing = Ping <$> (string "PING " *> many notSpace)

pQuit :: Parser ClientMsg
pQuit = const Quit <$> (string "QUIT " *> many anyChar)

pNick :: Parser ClientMsg
pNick = Nick <$> (string "NICK " *> many letter)

notSpace :: Parser Char
notSpace = noneOf [' ']

pUser :: Parser ClientMsg
pUser = do
  string "USER "
  user <- many notSpace
  space
  host <- many notSpace
  space
  server <- many notSpace
  space
  realName <- char ':' *> many (letter <|> char ' ')
  return $ NewUser user host server realName

getClientMsg :: ByteString -> Either ParseError ClientMsg
getClientMsg msgStr = parse pClientMsg "client msg" (C8.unpack msgStr)
