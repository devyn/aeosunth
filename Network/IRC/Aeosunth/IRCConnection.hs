{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Aeosunth.IRCConnection
  ( IRCProfile(..)
  , IRCConnection(connProfile, connState, connInput, connOutput)
  , IRCState
  , ircConnection
  )
 where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM

import           Data.Attoparsec.Text (IResult(..), parse)

import qualified Data.ByteString as ByteString

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

import           Network.Socket hiding (recv)
import           Network.Socket.ByteString (sendAll, recv)

import           Network.IRC.Aeosunth.Parser
import           Network.IRC.Aeosunth.Protocol

data IRCProfile = IRCProfile { ircHost     :: String
                             , ircPort     :: Int
                             , ircSSL      :: Bool -- not implemented yet
                             , ircNick     :: String
                             , ircUserName :: String
                             , ircHostName :: String
                             , ircServName :: String
                             , ircRealName :: String
                             , ircPass     :: Maybe String
                             , ircChannels :: [String]
                             }
                deriving (Show, Eq)

data IRCConnection = IRCConnection { connProfile :: IRCProfile
                                   , connState   :: TVar IRCState
                                   , connInput   :: TChan Command
                                   , connOutput  :: TChan ServerMessage
                                   }

type IRCState = ()
defaultState = ()

connectTCP :: String -> Int -> IO Socket

connectTCP host port = do addr   <- head <$> getAddrInfo (Just hints) (Just host) (Just (show port))
                          socket <- socket (addrFamily addr) Stream defaultProtocol
                          connect socket $ addrAddress addr
                          return socket
         where hints = defaultHints { addrFlags = [AI_PASSIVE, AI_NUMERICSERV] }

ircConnection :: IRCProfile -> IO IRCConnection

ircConnection profile = do state  <- newTVarIO defaultState
                           input  <- newTChanIO
                           output <- newTChanIO

                           let conn = IRCConnection profile state input output

                           socket <- connectTCP (ircHost profile) (ircPort profile)

                           forkIO $ ircConnectionHandler conn socket

                           return conn

ircConnectionHandler :: IRCConnection -> Socket -> IO ()

ircConnectionHandler conn socket   = do ircIdentify conn -- we do this first to ensure identification is the first thing done
                                        thr <- forkIO $ ircConnectionWriteHandler (connInput conn) socket
                                        ircJoinDefaultChannels conn
                                        ircReadLoop Text.empty
                                        killThread thr
                                        atomically $ writeTVar (connState conn) defaultState
                                        -- Reconnect to the server.
                                        socket' <- connectTCP (ircHost profile) (ircPort profile)
                                        ircConnectionHandler conn socket'

  where profile                     = connProfile conn
        getMore                     = recv socket 1024 >>= \ t ->
                                        return $ if ByteString.null t
                                                    then Nothing
                                                    else Just $ Text.decodeUtf8With Text.lenientDecode t

        parseLoop (Partial f)       = getMore >>= maybe (return ()) (parseLoop . f)
        parseLoop (Fail buffer _ _) = ircReadLoop . Text.drop 1 $ Text.dropWhile (/= '\n') buffer
        parseLoop (Done buffer r)   = ircHandleInput conn r >> ircReadLoop buffer
        ircReadLoop buffer          = parseLoop (parse serverMessage buffer)

ircIdentify :: IRCConnection -> IO ()

ircIdentify conn = atomically $ do unGetTChan cin (Command "USER" (map  (Text.pack . ($ profile))
                                                                        [ircUserName, ircHostName, ircServName])
                                                                  (Just . Text.pack $ ircRealName profile))
                                   unGetTChan cin (Command "NICK" [Text.pack (ircNick profile)] Nothing)
                                   maybe (return ()) (unGetTChan cin . Command "PASS" [] . Just . Text.pack) (ircPass profile)

                   where cin     = connInput conn
                         profile = connProfile conn

ircJoinDefaultChannels :: IRCConnection -> IO ()

ircJoinDefaultChannels conn
  = atomically . writeTChan (connInput conn) $ Command "JOIN" (map Text.pack . ircChannels $ connProfile conn) Nothing

ircConnectionWriteHandler :: TChan Command -> Socket -> IO ()

ircConnectionWriteHandler cin socket = do cmd <- atomically $ readTChan cin
                                          isw <- isWritable socket
                                          if not isw
                                             then atomically $ unGetTChan cin cmd -- quit, but ensure command isn't consumed
                                             else do sendAll socket . Text.encodeUtf8 $ serializeCommand cmd
                                                     ircConnectionWriteHandler cin socket

ircHandleInput :: IRCConnection -> ServerMessage -> IO ()

ircHandleInput conn (MessageFrom _ cmd@(Command "PING" l a))
  = atomically $ writeTChan (connInput conn) (Command "PONG" l a)

ircHandleInput conn sm = atomically $ writeTChan (connOutput conn) sm

if' :: a -> a -> Bool -> a

if' x _ True  = x
if' _ y False = y
