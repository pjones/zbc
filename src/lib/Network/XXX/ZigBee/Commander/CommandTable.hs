{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

module Network.XXX.ZigBee.Commander.CommandTable
  ( CommandTable,
    defaultCommandTable,
    resolve,
    lookup,
  )
where

import Control.Arrow (first)
import Control.Monad (forM)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Functor.Identity
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as Vector
import Network.XXX.ZigBee.Commander.Address
import Network.XXX.ZigBee.Commander.Command
import Network.XXX.ZigBee.Commander.Internal.Resolve
import Network.XXX.ZigBee.Commander.NodeTable hiding (lookup, resolve)
import qualified Network.XXX.ZigBee.Commander.NodeTable as NodeTable
import Prelude hiding (lookup)

data CommandTable' a = CommandTable (Map Text (a Address, Command))

type CommandTable = CommandTable' Identity

newtype Entry a = Entry {unEntry :: (Text, (a Address, Command))}

instance (FromUnresolved a) => FromJSON (CommandTable' a) where
  parseJSON (Array a) = do
    entries <- mapM parseJSON (Vector.toList a)
    return . CommandTable $ Map.fromList (map unEntry entries)
  parseJSON invalid = typeMismatch "command table" invalid

instance (FromUnresolved a) => FromJSON (Entry a) where
  parseJSON o@(Object v) = do
    name <- v .: "name"
    addr <- parseUnresolved =<< (v .: "node")
    cmd <- parseJSON o
    return $ Entry (name, (addr, cmd))
  parseJSON invalid = typeMismatch "command table entry" invalid

defaultCommandTable :: CommandTable
defaultCommandTable = CommandTable Map.empty

resolve ::
  NodeTable ->
  CommandTable' Unresolved ->
  Either String CommandTable
resolve nodes (CommandTable table) = CommandTable . Map.fromList <$> tryResolve
  where
    tryResolve :: Either String [(Text, (Identity Address, Command))]
    tryResolve =
      forM (Map.assocs table) $ \(name, (unresolved, command)) -> do
        addr <- NodeTable.resolve nodes unresolved
        return (name, (Identity addr, command))

lookup :: CommandTable -> Text -> Maybe (Address, Command)
lookup (CommandTable m) key = first runIdentity <$> Map.lookup key m
