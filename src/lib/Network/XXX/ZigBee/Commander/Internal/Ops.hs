{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

module Network.XXX.ZigBee.Commander.Internal.Ops
  ( send,
    reset,
  )
where

import Control.Concurrent
import Control.Monad (forM_)
import Data.Monoid
import Data.Text (Text)
import Network.XXX.ZigBee.Commander.Address
import qualified Network.XXX.ZigBee.Commander.CommandTable as CommandTable
import Network.XXX.ZigBee.Commander.Config
import Network.XXX.ZigBee.Commander.Event
import Network.XXX.ZigBee.Commander.Internal.Commander
import Network.XXX.ZigBee.Commander.Internal.Environment
import qualified Network.XXX.ZigBee.Commander.NodeTable as NodeTable

-- | Send a command to a device, given the name of the command.
send :: (MonadIO m) => Text -> Commander m ()
send name = do
  env <- ask

  let cmds = cCommandTable (config env)
      chan = commands env

  case CommandTable.lookup cmds name of
    Nothing -> return ()
    Just cmd -> do
      logger ("sending command: " <> name)
      liftIO (writeChan chan cmd)

-- | Reset a specific node if given a node name, otherwise all nodes
-- are reset.  Nodes are reset by sending a @ResetRequested@ event to
-- the server which in turn simulates a node joining the network.
-- This will cause its initialization code in the configuration file
-- to be executed.
reset :: (MonadIO m) => Maybe Text -> Commander m ()
reset name = do
  env <- ask

  let nodes = cNodeTable (config env)
      chan = events env

  case NodeTable.lookupOneOrAll nodes name of
    Left e -> loggerS e
    Right addrs -> forM_ addrs $ \addr -> do
      loggerS ("resetting node " <> show addr)
      liftIO (writeChan chan (ResetRequested (Network addr)))
