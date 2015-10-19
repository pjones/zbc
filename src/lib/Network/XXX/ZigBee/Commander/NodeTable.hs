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
module Network.XXX.ZigBee.Commander.NodeTable
       ( NodeTable
       , defaultNodeTable
       , resolve
       , lookup
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
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

--------------------------------------------------------------------------------
-- | Look-up table that maps node names to their MAC addresses.
newtype NodeTable = NodeTable { unNT :: Map Text MAC }

--------------------------------------------------------------------------------
-- | An entry in the table.
newtype Entry = Entry { unEntry :: (Text, MAC) }

--------------------------------------------------------------------------------
instance FromJSON Entry where
  parseJSON (Object v) = Entry <$> ((,) <$> v .: "name" <*> v .: "addr")
  parseJSON invalid    = typeMismatch "Node table entry" invalid

--------------------------------------------------------------------------------
instance FromJSON NodeTable where
  parseJSON (Array a) = do
    entries <- mapM parseJSON (Vector.toList a)
    return (makeTable entries)
  parseJSON invalid = typeMismatch "Node table" invalid

--------------------------------------------------------------------------------
makeTable :: [Entry] -> NodeTable
makeTable xs = NodeTable
  (Map.fromList (map unEntry xs) `Map.union` unNT defaultNodeTable)

--------------------------------------------------------------------------------
-- | Default node name to MAC table.
defaultNodeTable :: NodeTable
defaultNodeTable = NodeTable $ Map.fromList
  [ ("coordinator", coordinator)
  , ("broadcast",   broadcast)
  ]

--------------------------------------------------------------------------------
resolve :: NodeTable -> ParsedAddress -> Either String Address
resolve _   (Resolved addr)   = Right addr
resolve tbl (Unresolved name) =
  case lookup tbl name of
    Just mac -> Right (Network mac)
    Nothing  -> case parseMAC name of
                  Right mac -> Right (Network mac)
                  Left e    -> Left ("unknown node name or invalid MAC" ++ e)

--------------------------------------------------------------------------------
-- | Look up a node's MAC address using its name.
lookup :: NodeTable -> Text -> Maybe MAC
lookup (NodeTable m) key = Map.lookup key m
