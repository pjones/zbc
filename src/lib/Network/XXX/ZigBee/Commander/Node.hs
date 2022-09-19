{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

module Network.XXX.ZigBee.Commander.Node
  ( Node (..),
    NodeType (..),
    NodeName,
    GPIOs,
    newNode,
    nodeTypeFromDeviceType,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock
import Data.Word (Word8)
import Network.XXX.ZigBee.Commander.Address
import Network.XXX.ZigBee.Commander.GPIO

data Node = Node
  { nodeAddress :: Address,
    nodeType :: NodeType,
    nodeGPIOs :: GPIOs,
    nodeMutedUntil :: Maybe UTCTime
  }

data NodeType
  = NetworkCoordinator
  | NetworkRouter
  | NetworkEndpoint
  deriving (Show)

type NodeName = Text

-- | Mapping between GPIO names and the GPIO value that represents them.
type GPIOs = Map Text GPIO

newNode :: Address -> Node
newNode addr =
  Node
    { nodeAddress = addr,
      nodeType = NetworkEndpoint,
      nodeGPIOs = Map.empty,
      nodeMutedUntil = Nothing
    }

nodeTypeFromDeviceType :: Word8 -> NodeType
nodeTypeFromDeviceType 0 = NetworkCoordinator
nodeTypeFromDeviceType 1 = NetworkRouter
nodeTypeFromDeviceType _ = NetworkEndpoint
