{-# LANGUAGE ScopedTypeVariables #-}


{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Network.XXX.ZigBee.Commander.Local
       ( local
       , connect
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Data.Time.Calendar
import Data.Time.Clock
import System.IO

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Config
import Network.XXX.ZigBee.Commander.Device
import Network.XXX.ZigBee.Commander.Internal.Commander

--------------------------------------------------------------------------------
-- | Local ZigBee device (direct serial connection).
data Local = DisconnectedLocal FilePath UTCTime
             -- ^ Device hasn't been opened, or last open failed.

           | ConnectionPendingLocal FilePath Handle
             -- ^ Device opened, wanting to receive status.

           | ConnectedLocal FilePath Handle Device
             -- ^ Device opened and active.

--------------------------------------------------------------------------------
-- | Prepare a local connection.
local :: FilePath -> Local
local path = DisconnectedLocal path (UTCTime day t)
  where
    day :: Day
    day = ModifiedJulianDay 0

    t :: DiffTime
    t = fromIntegral (0 :: Integer)

--------------------------------------------------------------------------------
connect :: (MonadIO m) => Local -> Commander m Local
connect x =
  case x of
    DisconnectedLocal path t   -> connect' path t
    ConnectionPendingLocal {}  -> return x
    ConnectedLocal {}          -> return x

  where
    connect' :: (MonadIO m) => FilePath -> UTCTime -> Commander m Local
    connect' path t = do
      now  <- liftIO getCurrentTime
      wait <- fromIntegral <$> asks cConnectionRetryTimeout

      if diffUTCTime now t < wait
        then return x
        else do
          h   <- liftIO (openFile path ReadWriteMode) -- Open in binary mode
          liftIO (hSetBuffering h NoBuffering)
          return $ ConnectionPendingLocal path h
