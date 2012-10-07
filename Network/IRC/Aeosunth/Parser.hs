{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Aeosunth.Parser
  ( serverMessage
  )
 where

import           Prelude hiding (takeWhile)

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Text (Text)
import qualified Data.Text as Text

import           Network.IRC.Aeosunth.Protocol

serverMessage :: Parser ServerMessage

serverMessage       = many crlf *> (reply <|> messageFrom) <* crlf    <?> "serverMessage"
  where reply       = Reply <$> decimal <*> params <*> optional body  <?> "reply"
        messageFrom = MessageFrom <$> optional sender <*> command     <?> "messageFrom"

command :: Parser Command

command = Command <$> takeWhile1 (inClass "a-zA-Z") <*> params <*> optional body  <?> "command"

sender :: Parser Sender

sender         = char ':' *> (try (user <* char ' ') <|> (server <* char ' ')) <* many (char ' ')  <?> "sender"
  where isAcc  = inClass "a-zA-Z0-9[]\\`^{}-"
        servP  = takeWhile1 (notInClass " \0\r\n")
        server = Server <$> servP
        user   = User   <$> takeWhile1 isAcc
                        <*> optional (char '!' *> takeWhile1 (notInClass "@ \0\r\n"))
                        <*> optional (char '@' *> servP)
                        <?> "user"

params :: Parser [Text]

params = (:) <$> (many1 (char ' ') *> (Text.cons <$> satisfy (notInClass ": \0\r\n")
                  <*> takeWhile (notInClass " \0\r\n")))
             <*> params
     <|> pure []
     <?> "params"

body :: Parser Text

body = many1 (char ' ') *> char ':' *> takeWhile (notInClass "\0\r\n")  <?> "body"

crlf :: Parser ()

crlf = string "\r\n" *> pure ()  <?> "crlf"

dropLine :: Text -> Text

dropLine = Text.drop 1 . Text.dropWhile (/= '\n')
