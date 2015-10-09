{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Network.XXX.ZigBee.Commander.Internal.State
       ( State (..)
       , DeviceNodeState (..)
       , initialState
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Data.Time.Calendar
import Data.Time.Clock
import qualified Network.Protocol.ZigBee.ZNet25 as Z
import System.IO

--------------------------------------------------------------------------------
data State = State
  { deviceState  :: DeviceNodeState
  , decoderState :: Z.DecoderState
  }

--------------------------------------------------------------------------------
data DeviceNodeState = DeviceNodeState FilePath (Either UTCTime Handle)

--------------------------------------------------------------------------------
initialState :: FilePath -> State
initialState path = State { deviceState  = initialDeviceState path
                          , decoderState = Z.initDecode
                          }

--------------------------------------------------------------------------------
initialDeviceState :: FilePath -> DeviceNodeState
initialDeviceState path = DeviceNodeState path (Left initialTime)
  where
    initialTime :: UTCTime
    initialTime =  UTCTime (ModifiedJulianDay 0) (fromIntegral (0 :: Integer))
