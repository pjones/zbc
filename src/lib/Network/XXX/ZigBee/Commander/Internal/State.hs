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
       , DeviceStatus (..)
       , initialState
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Clock
import qualified Network.Protocol.ZigBee.ZNet25 as Z
import System.IO

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Address
import Network.XXX.ZigBee.Commander.Node

--------------------------------------------------------------------------------
data State = State
  { deviceStatus :: DeviceStatus
  , decoderState :: Z.DecoderState
  , frameID      :: Z.FrameId
  , nodeStatus   :: Map Address Node
  }

--------------------------------------------------------------------------------
data DeviceStatus = DeviceStatus Text (Either UTCTime Handle)

--------------------------------------------------------------------------------
initialState :: Text -> State
initialState path = State { deviceStatus = initialDeviceState path
                          , decoderState = Z.initDecode
                          , frameID      = 0
                          , nodeStatus   = Map.empty
                          }

--------------------------------------------------------------------------------
initialDeviceState :: Text -> DeviceStatus
initialDeviceState path = DeviceStatus path (Left initialTime)
  where
    initialTime :: UTCTime
    initialTime =  UTCTime (ModifiedJulianDay 0) (fromIntegral (0 :: Integer))
