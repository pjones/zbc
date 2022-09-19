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
import Control.Exception (finally)
import Control.Monad (forever, void)
import Data.Aeson hiding (decode)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteStringL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network
import Network.XXX.ZigBee.Commander.Internal.Commander
import qualified Network.XXX.ZigBee.Commander.Internal.Ops as Ops
import System.Directory
import System.FilePath
import System.IO
import Text.Parsec
import Text.Parsec.String

-- | Opaque data type for referring to a client connection.
data Client = Client Handle

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
  let handler = void . runCommander env . listen

  liftIO $ do
    path <- serverSocketName
    socket <- Network.listenOn (Network.UnixSocket path)
    handler socket `finally` cleanup socket path
  where
    cleanup :: Network.Socket -> FilePath -> IO ()
    cleanup socket path = do
      Network.sClose socket
      removeFile path

    listen :: (MonadIO m) => Network.Socket -> Commander m ()
    listen socket = do
      env <- ask
      let handler = runCommander env . processRequests

      forever $
        liftIO $ do
          (h, _, _) <- Network.accept socket
          hSetBuffering h LineBuffering
          async (handler h `finally` hClose h)

    processRequests :: (MonadIO m) => Handle -> Commander m ()
    processRequests h = forever $ do
      request <- liftIO (ByteStringL.fromStrict <$> ByteString.hGetLine h)
      mapM_ dispatch (Aeson.decode request)

    dispatch :: (MonadIO m) => RemoteRequest -> Commander m ()
    dispatch (SendCommand name) = Ops.send name
    dispatch (ResetNode name) = Ops.reset name

client :: (Client -> IO a) -> IO a
client f = do
  path <- serverSocketName
  socket <- Network.connectTo "localhost" (Network.UnixSocket path)
  hSetBuffering socket LineBuffering
  f (Client socket) `finally` hClose socket

send :: Client -> RemoteRequest -> IO ()
send (Client socket) request = do
  ByteStringL.hPut socket (encode request)
  hFlush socket

-- | Returns the file path for the Unix Domain Socket.
serverSocketName :: IO FilePath
serverSocketName = do
  directory <- getXdgDirectory XdgData "zbc"
  createDirectoryIfMissing True directory
  return (directory </> "zbc.socket")
