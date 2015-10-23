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
       ( FromUnresolved (..)
       , Unresolved (..)
       , resolveMismatch
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)

--------------------------------------------------------------------------------
-- | A bit of a hack to allow JSON parsing of types with kind
-- @(* -> *)@ such as the @Unresolved@ type.
class FromUnresolved f where
  parseUnresolved :: FromJSON a => Value -> Parser (f a)

--------------------------------------------------------------------------------
-- | A type that represents a value that may not have been fully
-- parsed from JSON.
data Unresolved a = Resolved a
                    -- ^ Fully parsed.

                  | UnresolvedText Text
                    -- ^ Not parsed, but preserved JSON text.

                  | UnresolvedValue Value
                    -- ^ Not parsed, but preserved JSON value.

--------------------------------------------------------------------------------
instance FromUnresolved Unresolved where
  parseUnresolved s@(String t) =
    Resolved <$> parseJSON s <|> pure (UnresolvedText t)
  parseUnresolved invalid =
    pure (UnresolvedValue invalid)

--------------------------------------------------------------------------------
resolveMismatch :: String -> Value -> String
resolveMismatch = undefined
