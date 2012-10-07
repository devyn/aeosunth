module Network.IRC.Aeosunth.Protocol
  ( ServerMessage(..)
  , Sender(..)
  , Command(..)
  )
 where

import           Data.Text (Text)

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
