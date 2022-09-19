{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

module Network.XXX.ZigBee.Commander.Internal.Commander
  ( Commander,
    logger,
    loggerS,
    debug,
    nextFrameID,
    isMuted,
    mute,
    spawn,
    runCommander,
    MonadIO,
    liftIO,
    ask,
    asks,
  )
where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock
import Data.Time.Format
import qualified Network.Protocol.ZigBee.ZNet25 as Z
import Network.XXX.ZigBee.Commander.Address
import Network.XXX.ZigBee.Commander.Internal.Environment
import Network.XXX.ZigBee.Commander.Internal.State
import Network.XXX.ZigBee.Commander.Node
import System.IO

newtype Commander m a = Commander {unC :: ReaderT Environment (ExceptT String m) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Environment)

-- FIXME:
logger :: (MonadIO m) => Text -> Commander m ()
logger = liftIO . Text.hPutStrLn stderr

loggerS :: (MonadIO m) => String -> Commander m ()
loggerS = logger . Text.pack

-- FIXME:
debug :: (Monad m) => Commander m () -> Commander m ()
debug = when True

nextFrameID :: (MonadIO m) => Commander m Z.FrameId
nextFrameID = do
  stateVar <- asks state

  liftIO . atomically $ do
    s <- readTVar stateVar

    let fid =
          if frameID s == maxBound
            then 1
            else frameID s + 1

    writeTVar stateVar s {frameID = fid}
    return fid

-- | Returns true if the node with the given address is muted.
isMuted :: (MonadIO m) => Address -> Commander m Bool
isMuted addr = do
  stateVar <- asks state
  table <- nodeStatus <$> liftIO (readTVarIO stateVar)

  case nodeMutedUntil =<< Map.lookup addr table of
    Nothing -> return False
    Just t -> (t >) <$> liftIO getCurrentTime

-- | Mark a node as muted.
mute :: (MonadIO m) => Address -> Int -> Commander m ()
mute addr msecs = do
  stateVar <- asks state
  now <- liftIO getCurrentTime
  let time = addUTCTime mkndt now

  liftIO . atomically . modifyTVar stateVar $ \s ->
    let table = nodeStatus s
        table' = setNode time (getNode table) table
     in s {nodeStatus = table'}

  debug $
    let locale = defaultTimeLocale
        format = "%H:%M:%S"
     in loggerS $ show addr ++ " is muted until " ++ formatTime locale format time
  where
    mkndt :: NominalDiffTime
    mkndt = realToFrac . picosecondsToDiffTime . fromIntegral $ (msecs * 1000000000)

    getNode :: Map Address Node -> Node
    getNode table = fromMaybe (newNode addr) (Map.lookup addr table)

    setNode :: UTCTime -> Node -> Map Address Node -> Map Address Node
    setNode t node = Map.insert addr (setMuted t node)

    setMuted :: UTCTime -> Node -> Node
    setMuted t node = node {nodeMutedUntil = Just t}

spawn :: Commander IO a -> Commander IO (Async (Either String a))
spawn action = liftIO . async . flip runCommander action =<< ask

runCommander ::
  (Monad m) =>
  Environment ->
  Commander m a ->
  m (Either String a)
runCommander env cmdr = do
  result <- runExceptT $ runReaderT (unC cmdr) env

  return $ case result of
    Left e -> Left e
    Right a -> Right a
