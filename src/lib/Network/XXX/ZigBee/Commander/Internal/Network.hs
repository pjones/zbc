{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

module Network.XXX.ZigBee.Commander.Internal.Network
  ( Client,
    RemoteRequest (..),
    server,
    client,
    send,
    parseRemoteRequest,
  )
where

import Control.Concurrent.Async
import Control.Exception (bracket)
import Control.Monad (forever, unless, void, when)
import Data.Aeson hiding (decode)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as ByteStringL
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.Socket as Network
import qualified Network.Socket.ByteString as Network
import Network.XXX.ZigBee.Commander.Internal.Commander
import qualified Network.XXX.ZigBee.Commander.Internal.Ops as Ops
import System.Directory
import System.FilePath
import System.IO
import Text.Parsec
import Text.Parsec.String

-- | Opaque data type for referring to a client connection.
newtype Client = Client Network.Socket

data RemoteRequest
  = -- | Send a command.
    SendCommand Text
  | -- | Reset a node or all nodes.
    ResetNode (Maybe Text)

instance FromJSON RemoteRequest where
  parseJSON (Object v) = do
    command <- v .: "command"

    case Text.toLower command of
      "send" -> SendCommand <$> (v .: "name")
      "reset" -> ResetNode <$> (v .:? "name")
      _ -> fail ("invalid request: " ++ Text.unpack command)
  parseJSON invalid = typeMismatch "remote request" invalid

instance ToJSON RemoteRequest where
  toJSON rr = case rr of
    SendCommand name ->
      object
        [ "command" .= ("send" :: String),
          "name" .= name
        ]
    ResetNode name ->
      object
        [ "command" .= ("reset" :: String),
          "name" .= name
        ]

-- | Parse a string containing a textual form of a 'RemoteRequest'.
parseRemoteRequest :: String -> Maybe RemoteRequest
parseRemoteRequest s =
  case parse requests s s of
    Left _ -> Nothing
    Right r -> Just r
  where
    requests :: Parser RemoteRequest
    requests = choice [try remoteSend, remoteReset] <* eof

    remoteSend :: Parser RemoteRequest
    remoteSend = do
      void (string "send")
      skipMany space
      SendCommand . Text.pack <$> many1 anyChar

    remoteReset :: Parser RemoteRequest
    remoteReset = do
      void (string "reset")
      skipMany space
      ResetNode <$> ((Just . Text.pack <$> many1 anyChar) <|> pure Nothing)

server :: (MonadIO m) => Commander m ()
server = do
  env <- ask
  path <- serverSocketName
  liftIO $ bracket (open path) (close path) (void . runCommander env . listen)
  where
    open :: FilePath -> IO Network.Socket
    open path = do
      createDirectoryIfMissing True (takeDirectory path)
      sock <- Network.socket Network.AF_UNIX Network.Stream 0
      Network.bind sock $ Network.SockAddrUnix path
      Network.listen sock Network.maxListenQueue
      pure sock

    close :: FilePath -> Network.Socket -> IO ()
    close path socket = do
      Network.close socket
      doesPathExist path >>= flip when (removeFile path)

    listen :: (MonadIO m) => Network.Socket -> Commander m ()
    listen socket = do
      env <- ask

      forever $
        liftIO $ do
          (clientSocket, _clientAddr) <- Network.accept socket
          async (runCommander env $ processRequests clientSocket)

    processRequests :: (MonadIO m) => Network.Socket -> Commander m ()
    processRequests socket =
      let read previous = do
            (buffer, leftovers) <-
              first (previous <>) . ByteString.break (== '\n')
                <$> liftIO (Network.recv socket 4096)
            unless (ByteString.null buffer) $ do
              traverse_ dispatch (Aeson.decode $ ByteStringL.fromStrict buffer)
              read leftovers
       in read ""

    dispatch :: (MonadIO m) => RemoteRequest -> Commander m ()
    dispatch (SendCommand name) = Ops.send name
    dispatch (ResetNode name) = Ops.reset name

client :: (Client -> IO a) -> IO a
client f = do
  path <- serverSocketName
  socket <- Network.socket Network.AF_UNIX Network.Stream 0
  Network.connect socket $ Network.SockAddrUnix path
  f (Client socket)

send :: Client -> RemoteRequest -> IO ()
send (Client socket) request =
  Network.sendMany socket (ByteStringL.toChunks $ encode request)

-- | Returns the file path for the Unix Domain Socket.
serverSocketName :: MonadIO m => m FilePath
serverSocketName = liftIO $ do
  directory <- getXdgDirectory XdgData "zbc"
  createDirectoryIfMissing True directory
  return (directory </> "zbc.socket")
