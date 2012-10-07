{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Aeosunth.Protocol
  ( ServerMessage(..)
  , Sender(..)
  , Command(..)
  , serializeCommand
  )
 where

import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text

data ServerMessage = MessageFrom (Maybe Sender) Command
                   | Reply { replyCode   :: Int
                           , replyParams :: [Text]
                           , replyBody   :: Maybe Text
                           }
                   deriving (Show, Eq)

data Sender = Server { serverName :: Text }
            | User   { nickName   :: Text
                     , userName   :: Maybe Text
                     , hostName   :: Maybe Text
                     }
            deriving (Show, Eq)

data Command = Command { commandName   :: Text
                       , commandParams :: [Text]
                       , commandBody   :: Maybe Text
                       }
             deriving (Show, Eq)

serializeCommand :: Command -> Text

serializeCommand (Command name params body)
  = Text.append (Text.concat . intersperse " " $ name : params ++ maybe [] ((: []) . Text.cons ':') body) "\r\n"
