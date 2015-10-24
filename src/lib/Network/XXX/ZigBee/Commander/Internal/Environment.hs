{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Network.XXX.ZigBee.Commander.Internal.Environment
       ( Environment (..)
       , newEnvironment
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Control.Concurrent
import Control.Concurrent.STM

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Address
import Network.XXX.ZigBee.Commander.Command
import Network.XXX.ZigBee.Commander.Config
import Network.XXX.ZigBee.Commander.Event
import Network.XXX.ZigBee.Commander.Internal.State

--------------------------------------------------------------------------------
-- | The environment that the commander server runs in.  It's a
-- read-only environment but some of the fields are transactional
-- variables.
data Environment = Environment
  { config   :: Config
  , events   :: Chan Event
  , commands :: Chan (Address, Command)
  , state    :: TVar State
  }

--------------------------------------------------------------------------------
-- | Create a brand new environment that can be shared among many
-- concurrent threads.  This should only be called once per server
-- instance.
newEnvironment :: Config -> IO Environment
newEnvironment c = Environment <$> pure c
                               <*> newChan
                               <*> newChan
                               <*> newTVarIO (initialState $ cDevice c)
