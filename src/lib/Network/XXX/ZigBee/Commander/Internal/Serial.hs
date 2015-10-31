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
import Control.Concurrent.STM
import Control.Monad (forever)
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
import Network.XXX.ZigBee.Commander.Event
import Network.XXX.ZigBee.Commander.Internal.Commander
import Network.XXX.ZigBee.Commander.Internal.Environment
import Network.XXX.ZigBee.Commander.Internal.State
import Network.XXX.ZigBee.Commander.Internal.Util

--------------------------------------------------------------------------------
-- | Process incoming and outgoing frames in a separate thread.
serialThread :: (MonadIO m) => Commander m ()
serialThread = forever $ do
  eventsChan   <- asks events
  commandsChan <- asks commands
  readThread   <- waitRead
  writeThread  <- liftIO (async $ readChan commandsChan)
  action       <- liftIO (waitEitherCancel readThread writeThread)

  case action of
    Left _  -> reader >>= liftIO . writeList2Chan eventsChan
    Right f -> writer f

  where
    -- Wait for data from the locally connected ZigBee device to be
    -- available.  If no device is connected, wait for it to become
    -- connected.
    waitRead :: (MonadIO m) => Commander m (Async Bool)
    waitRead  = withConnectedDevice $ \mh ->
      case mh of
        Nothing -> liftIO (threadDelay 1000000) >> waitRead
        Just h  -> liftIO (async $ hWaitForInput h (-1))

--------------------------------------------------------------------------------
device :: (MonadIO m) => Commander m DeviceStatus
device = do
  status <- deviceStatus <$> (asks state >>= liftIO . atomically . readTVar)

  case status of
    DeviceStatus _ (Right _)   -> return status
    DeviceStatus path (Left t) -> connect path t >>=
                                  maybe (return status) connected
  where
    connect :: (MonadIO m) => Text -> UTCTime -> Commander m (Maybe DeviceStatus)
    connect path t = do
      now  <- liftIO getCurrentTime
      secs <- fromIntegral <$> asks (cConnectionRetryTimeout . config)

      if diffUTCTime now t < secs
        then return Nothing
          else do
          -- FIXME: Guard for exceptions.
          -- FIXME: Store serial port settings in Config.
          h <- liftIO (hOpenSerial (Text.unpack path) defaultSerialSettings)
          return . Just $ DeviceStatus path (Right h)

    -- FIXME: Send command to get MAC address of locally connected node.
    connected :: (MonadIO m) => DeviceStatus -> Commander m DeviceStatus
    connected status = do
      let update s = s {deviceStatus = status}
      stateVar <- asks state
      liftIO (atomically $ modifyTVar stateVar update)
      writer (Local, AT (mkATCode (78, 68)) Nothing) -- AT-ND (node discovery)
      return status

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
-- | Send a single command to one of the nodes on the network.
writer :: (MonadIO m) => (Address, Command) -> Commander m ()
writer (addr, cmd) = withConnectedDevice go
  where
    go :: (MonadIO m) => Maybe Handle -> Commander m ()
    go Nothing  = logger "not connected, dropping frames"
    go (Just h) = do
      debug (loggerS ("sending frame: " ++ show cmd) >>
             loggerS ("   to address: " ++ show addr))

      fid <- nextFrameID
      write h (Z.encode $ mkFrame fid addr cmd)

    -- FIXME: add support for hPutNonBlocking by maintaining a write
    -- buffer and trying to flush that buffer when called.
    write :: (MonadIO m) => Handle -> [ByteString] -> Commander m ()
    write h bs = let bs' = ByteString.concat bs
                  in liftIO (ByteString.hPut h bs') >>
                     debug (hexdump "wrote bytes: " bs')

--------------------------------------------------------------------------------
-- | Read events from the connected device.
reader :: (MonadIO m) => Commander m [Event]
reader = withConnectedDevice go
  where
    go :: (MonadIO m) => Maybe Handle -> Commander m [Event]
    go Nothing  = return []
    go (Just h) = do
      ds <- decoderState <$> (asks state >>= liftIO . atomically . readTVar)
      bs <- liftIO (ByteString.hGetSome h 1024)
      -- debug (hexdump "read bytes: " bs)

      let (results, decoderState') = runState (Z.decode bs) ds
          (errors,  frames)        = partitionEithers results
          readEvents               = concatMap frameToEvent frames

      updateDecoder decoderState'
      mapM_ (logger . Text.pack) errors
      debug (mapM_ (logger . Text.pack . show) frames)
      debug (mapM_ (logger . Text.pack . show) readEvents)
      return readEvents

    updateDecoder :: (MonadIO m) => Z.DecoderState -> Commander m ()
    updateDecoder ds = do
      let update s = s {decoderState = ds}
      stateVar <- asks state
      liftIO (atomically $ modifyTVar stateVar update)
