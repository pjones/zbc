{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Network.XXX.ZigBee.Commander.Internal.Dispatch
       ( dispatch
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Data.Maybe
import qualified Network.Protocol.ZigBee.ZNet25 as Z

--------------------------------------------------------------------------------
-- Local Imports:
import qualified Network.XXX.ZigBee.Commander.CommandTable as CommandTable
import Network.XXX.ZigBee.Commander.Config
import Network.XXX.ZigBee.Commander.Event
import Network.XXX.ZigBee.Commander.Internal.Commander

--------------------------------------------------------------------------------
dispatch :: (Monad m) => Z.Frame -> Commander m [Z.Frame]
dispatch = go . frameToEvent
  where
    ----------------------------------------------------------------------------
    go :: (Monad m) => Maybe Event -> Commander m [Z.Frame]
    go Nothing      = return []
    go (Just event) = do
      as <- concatMap eventActions <$> handlers event
      concat <$> mapM action as

    ----------------------------------------------------------------------------
    handlers :: (Monad m) => Event -> Commander m [EventHandler]
    handlers event = eventHandlers event . cEventHandlers <$> ask

    ----------------------------------------------------------------------------
    action :: (Monad m) => EventAction -> Commander m [Z.Frame]
    action ea = case ea of
      SendCommand name ->
        do cmds <- asks cCommandTable
           fid  <- nextFrameID
           return (maybeToList $ CommandTable.lookupFrame cmds fid name)
