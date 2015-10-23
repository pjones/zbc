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
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock
import qualified Network.Protocol.ZigBee.ZNet25 as Z
import System.Hardware.Serialport
import System.IO

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Address
import Network.XXX.ZigBee.Commander.Command
import Network.XXX.ZigBee.Commander.Config
import Network.XXX.ZigBee.Commander.Internal.Commander
import Network.XXX.ZigBee.Commander.Internal.State
import Network.XXX.ZigBee.Commander.Internal.Util

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
device :: (MonadIO m) => Commander m DeviceStatus
device = do
  s <- get
  let ns = deviceStatus s

  case ns of
    DeviceStatus _ (Right _)   -> return ns
    DeviceStatus path (Left t) -> connect path t >>=
                                  maybe (return ns) (connected s)
  where
    connect :: (MonadIO m) => Text -> UTCTime -> Commander m (Maybe DeviceStatus)
    connect path t = do
      now  <- liftIO getCurrentTime
      secs <- fromIntegral <$> asks cConnectionRetryTimeout

      if diffUTCTime now t < secs
        then return Nothing
          else do
          -- FIXME: Guard for exceptions.
          -- FIXME: Store serial port settings in Config.
          h <- liftIO (hOpenSerial (Text.unpack path) defaultSerialSettings)
          return . Just $ DeviceStatus path (Right h)

    -- FIXME: Send command to get MAC address of locally connected node.
    connected :: (MonadIO m) => State -> DeviceStatus -> Commander m DeviceStatus
    connected s ns = do fid <- nextFrameID
                        put s {deviceStatus = ns}
                        writer [nodeDiscovery fid]
                        return ns

    nodeDiscovery :: Z.FrameId -> Z.Frame -- Send an ATND command.
    nodeDiscovery fid = mkFrame fid Local (AT (mkATCode (78, 68)) Nothing)

--------------------------------------------------------------------------------
withConnectedDevice :: (MonadIO m)
                    => (Maybe Handle -> Commander m a) -> Commander m a
withConnectedDevice f = do
  ns <- device

  -- FIXME: Catch exceptions when calling `f' and then close the
  -- device and reset it back to a pending state.
  case ns of
    DeviceStatus _ (Left _)  -> f Nothing
    DeviceStatus _ (Right h) -> f (Just h)

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
    write h bs = let bs' = ByteString.concat bs
                  in liftIO (ByteString.hPut h bs') >>
                     debug (hexdump "wrote bytes: " bs')

--------------------------------------------------------------------------------
reader :: (MonadIO m) => Commander m [Z.Frame]
reader = withConnectedDevice go
  where
    go :: (MonadIO m) => Maybe Handle -> Commander m [Z.Frame]
    go Nothing  = return []
    go (Just h) = do
      s  <- get
      bs <- liftIO (ByteString.hGetSome h 1024)
      debug (hexdump "read bytes: " bs)

      let (results, decoderState') = runState (Z.decode bs) (decoderState s)
          (errors,  frames)        = partitionEithers results

      put s {decoderState = decoderState'}
      mapM_ (logger . Text.pack) errors
      return frames
