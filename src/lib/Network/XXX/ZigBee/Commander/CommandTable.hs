{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Network.XXX.ZigBee.Commander.CommandTable
       ( CommandTable
       , defaultCommandTable
       , resolve
       , lookup
       ) where

--------------------------------------------------------------------------------
import Control.Monad (forM)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Vector as Vector
import Prelude hiding (lookup)

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Address
import Network.XXX.ZigBee.Commander.Command
import qualified Network.XXX.ZigBee.Commander.NodeTable as NodeTable
import Network.XXX.ZigBee.Commander.NodeTable hiding (resolve, lookup)
import Network.XXX.ZigBee.Commander.Internal.Resolve

--------------------------------------------------------------------------------
data CommandTable a = CommandTable (Map Text (a, Command))

--------------------------------------------------------------------------------
newtype Entry a = Entry { unEntry :: (Text, (a, Command)) }

--------------------------------------------------------------------------------
instance (FromJSON a) => FromJSON (CommandTable a) where
  parseJSON (Array a) = do
    entries <- mapM parseJSON (Vector.toList a)
    return . CommandTable $ Map.fromList (map unEntry entries)
  parseJSON invalid = typeMismatch "command table" invalid

--------------------------------------------------------------------------------
instance (FromJSON a) => FromJSON (Entry a) where
  parseJSON o@(Object v) = do
    name <- v .: "name"
    addr <- v .: "node"
    cmd  <- parseJSON o
    return $ Entry (name, (addr, cmd))
  parseJSON invalid = typeMismatch "command table entry" invalid

--------------------------------------------------------------------------------
defaultCommandTable :: CommandTable Address
defaultCommandTable = CommandTable Map.empty

--------------------------------------------------------------------------------
resolve :: NodeTable
        -> CommandTable (Unresolved Address)
        -> Either String (CommandTable Address)
resolve nodes (CommandTable table) = CommandTable . Map.fromList <$> tryResolve
  where
    tryResolve :: Either String [(Text, (Address, Command))]
    tryResolve =
      forM (Map.assocs table) $ \(name, (unresolved, command)) -> do
        addr <- NodeTable.resolve nodes unresolved
        return (name, (addr, command))

--------------------------------------------------------------------------------
lookup :: CommandTable a -> Text -> Maybe (a, Command)
lookup (CommandTable m) key = Map.lookup key m
