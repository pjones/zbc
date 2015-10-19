{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Network.XXX.ZigBee.Commander.Config
       ( Config (..)
       , defaultConfig
       ) where

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.NodeTable

--------------------------------------------------------------------------------
data Config = Config
  { cDevice                 :: FilePath
  , cConnectionRetryTimeout :: Int
  , cNodeTable              :: NodeTable
  }

--------------------------------------------------------------------------------
defaultConfig :: Config
defaultConfig =
  Config { cDevice                 = "/dev/ttyUSB0"
         , cConnectionRetryTimeout = 5
         , cNodeTable              = defaultNodeTable
         }
