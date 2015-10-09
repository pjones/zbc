{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Network.XXX.ZigBee.Commander.Internal.Serial
       ( writer
       , reader
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Control.Monad.State (runState)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Either
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock
import qualified Network.Protocol.ZigBee.ZNet25 as Z
import System.IO

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Config
import Network.XXX.ZigBee.Commander.Internal.Commander
import Network.XXX.ZigBee.Commander.Internal.State

--------------------------------------------------------------------------------
device :: (MonadIO m) => Commander m DeviceNodeState
device = do
  s <- get
  let ns = deviceState s

  case ns of
    DeviceNodeState _ (Right _)   -> return ns
    DeviceNodeState path (Left t) -> do r <- connect path t
                                        maybe (return ns) (update s) r

  where
    update :: (Monad m) => State -> DeviceNodeState -> Commander m DeviceNodeState
    update s ns = put s {deviceState = ns} >> return ns

    connect :: (MonadIO m) => FilePath -> UTCTime -> Commander m (Maybe DeviceNodeState)
    connect path t = do
      now  <- liftIO getCurrentTime
      wait <- fromIntegral <$> asks cConnectionRetryTimeout

      if diffUTCTime now t < wait
        then return Nothing
          else do
          -- FIXME: Guard for exceptions.
          h   <- liftIO (openBinaryFile path ReadWriteMode)
          liftIO (hSetBuffering h NoBuffering)
          return . Just $ DeviceNodeState path (Right h)

--------------------------------------------------------------------------------
withConnectedDevice :: (MonadIO m)
                    => Maybe Text
                    -- ^ Error message to display if the local
                    -- device node is not currently connected.

                    -> (Handle -> Commander m ())
                    -- ^ Function to call when the local device node
                    -- is connected.

                    -> Commander m ()
                    -- ^ Result.
withConnectedDevice e f = do
  ns <- device

  case ns of
    DeviceNodeState _ (Left _)  -> maybe (return ()) logger e
    DeviceNodeState _ (Right h) -> f h -- FIXME: catch exceptions,
                                       -- disable device.

--------------------------------------------------------------------------------
writer :: (MonadIO m) => [Z.Frame] -> Commander m ()
writer frames = withConnectedDevice (Just "not connected, dropping frames") go
  where
    go :: (MonadIO m) => Handle -> Commander m ()
    go h = mapM_ (write h . Z.encode) frames

    -- FIXME: add support for hPutNonBlocking by maintaining a write
    -- buffer and trying to flush that buffer when called.
    write :: (MonadIO m) => Handle -> [ByteString] -> Commander m ()
    write h = liftIO . ByteString.hPut h . ByteString.concat

--------------------------------------------------------------------------------
reader :: (MonadIO m) => Commander m ()
reader = withConnectedDevice Nothing go
  where
    go :: (MonadIO m) => Handle -> Commander m ()
    go h = do
      s  <- get
      bs <- liftIO (ByteString.hGetNonBlocking h 1024)

      let (results, decoderState') = runState (Z.decode bs) (decoderState s)
          (errors,  frames)        = partitionEithers results

      put s {decoderState = decoderState'}
      mapM_ (logger . Text.pack) errors
      addFrames frames

    -- FIXME: Where do frames go after they are read?
    addFrames = undefined
