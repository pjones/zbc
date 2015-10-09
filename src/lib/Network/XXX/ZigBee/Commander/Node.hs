{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Network.XXX.ZigBee.Commander.Node
       ( Node (..)
       , NodeAddress (..)
       , GPIOs
       , remoteNode
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Address
import Network.XXX.ZigBee.Commander.GPIO

--------------------------------------------------------------------------------
data Node = Node
  { nodeAddress :: NodeAddress
  , nodeGPIOs   :: GPIOs
  }

--------------------------------------------------------------------------------
data NodeAddress = LocalNode
                   -- ^ FIXME:

                 | RemoteNode MAC
                   -- ^ A node on the ZigBee network with the
                   -- specified 'MAC' address.

--------------------------------------------------------------------------------
-- | Mapping between GPIO names and the GPIO value that represents them.
type GPIOs = Map Text GPIO

--------------------------------------------------------------------------------
remoteNode :: MAC -> Node
remoteNode mac =
  Node { nodeAddress = RemoteNode mac
       , nodeGPIOs   = Map.empty
       }
