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
data Config = Config
  { cConnectionRetryTimeout :: Int
  }

--------------------------------------------------------------------------------
defaultConfig :: Config
defaultConfig =
  Config { cConnectionRetryTimeout = 5
         }
