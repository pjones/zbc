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
module Network.XXX.ZigBee.Commander.Internal.Resolve
       ( Unresolved (..)
       , resolveMismatch
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Data.Aeson
--import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)

--------------------------------------------------------------------------------
data Unresolved a = UnresolvedText Text | UnresolvedValue Value

--------------------------------------------------------------------------------
instance FromJSON (Unresolved a) where
  parseJSON (String t) = return (UnresolvedText t)
  parseJSON invalid    = return (UnresolvedValue invalid)

--------------------------------------------------------------------------------
resolveMismatch :: String -> Value -> String
resolveMismatch = undefined
