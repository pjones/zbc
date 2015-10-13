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
       ( serialThread
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.State (runState)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Either
import Data.Monoid
import qualified Data.Text as Text
import Data.Time.Clock
import qualified Network.Protocol.ZigBee.ZNet25 as Z
import System.Hardware.Serialport
import System.IO
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Config
import Network.XXX.ZigBee.Commander.Internal.Commander
import Network.XXX.ZigBee.Commander.Internal.State

--------------------------------------------------------------------------------
-- | Process incoming and outgoing frames in a separate thread.  This
-- computation is expected to be wrapped in a @forever@ call.
serialThread :: (MonadIO m) => Chan Z.Frame -> Chan Z.Frame -> Commander m ()
serialThread inchan outchan = do
  readThread  <- waitRead
  writeThread <- waitWrite
  action      <- liftIO (waitEitherCancel readThread writeThread)

  case action of
    Left _  -> reader >>= liftIO . writeList2Chan outchan
    Right f -> writer [f]

  where
    -- Wait for data from the locally connected ZigBee device to be
    -- available.  If no device is connected, wait for it to become
    -- connected.
    waitRead :: (MonadIO m) => Commander m (Async Bool)
    waitRead  = withConnectedDevice $ \mh ->
      case mh of
        Nothing -> liftIO (threadDelay 1000000) >> waitRead
        Just h  -> liftIO (async $ hWaitForInput h (-1))

    -- Wait for another thread to ask for frames to be written.
    waitWrite :: (MonadIO m) => Commander m (Async Z.Frame)
    waitWrite = liftIO (async $ readChan inchan)

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
      secs <- fromIntegral <$> asks cConnectionRetryTimeout

      if diffUTCTime now t < secs
        then return Nothing
          else do
          -- FIXME: Guard for exceptions.
          -- FIXME: Store serial port settings in Config.
          h <- liftIO (hOpenSerial path defaultSerialSettings)
          return . Just $ DeviceNodeState path (Right h)

--------------------------------------------------------------------------------
withConnectedDevice :: (MonadIO m)
                    => (Maybe Handle -> Commander m a)
                    -- ^ Function to call when the local device node
                    -- is connected.

                    -> Commander m a
                    -- ^ Result.
withConnectedDevice f = do
  ns <- device

  case ns of
    DeviceNodeState _ (Left _)  -> f Nothing
    DeviceNodeState _ (Right h) -> f (Just h) -- FIXME: catch exceptions,
                                              -- disable device.

--------------------------------------------------------------------------------
writer :: (MonadIO m) => [Z.Frame] -> Commander m ()
writer frames = withConnectedDevice go
  where
    go :: (MonadIO m) => Maybe Handle -> Commander m ()
    go Nothing  = logger "not connected, dropping frames"
    go (Just h) = mapM_ (write h . Z.encode) frames

    -- FIXME: add support for hPutNonBlocking by maintaining a write
    -- buffer and trying to flush that buffer when called.
    write :: (MonadIO m) => Handle -> [ByteString] -> Commander m ()
    write h = liftIO . ByteString.hPut h . ByteString.concat

--------------------------------------------------------------------------------
reader :: (MonadIO m) => Commander m [Z.Frame]
reader = withConnectedDevice go
  where
    go :: (MonadIO m) => Maybe Handle -> Commander m [Z.Frame]
    go Nothing  = return []
    go (Just h) = do
      s  <- get
      bs <- liftIO (ByteString.hGetSome h 1024)
      hexdump bs -- FIXME: only when debugging output is enabled

      let (results, decoderState') = runState (Z.decode bs) (decoderState s)
          (errors,  frames)        = partitionEithers results

      put s {decoderState = decoderState'}
      mapM_ (logger . Text.pack) errors
      return frames

    hexdump :: (MonadIO m) => ByteString -> Commander m ()
    hexdump bs = let encoded = concatMap (printf "%02x ") (ByteString.unpack bs)
                  in logger ("read bytes: " <> Text.pack encoded)
