{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

module Network.XXX.ZigBee.Commander.Config
  ( Config (..),
    defaultConfig,
    readConfigFile,
  )
where

import Control.Monad (forM)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import qualified Data.Yaml as YAML
import Network.XXX.ZigBee.Commander.CommandTable (CommandTable)
import qualified Network.XXX.ZigBee.Commander.CommandTable as CommandTable
import Network.XXX.ZigBee.Commander.Event (EventHandler)
import qualified Network.XXX.ZigBee.Commander.Event as EventHandler
import Network.XXX.ZigBee.Commander.NodeTable (NodeTable)
import qualified Network.XXX.ZigBee.Commander.NodeTable as NodeTable

data Config = Config
  { cDevice :: Text,
    cConnectionRetryTimeout :: Int,
    cNodeTable :: NodeTable,
    cCommandTable :: CommandTable,
    cEventHandlers :: [EventHandler]
  }

instance FromJSON Config where
  parseJSON (Object v) = do
    addrs <- v .: "nodes"
    cmds <- v .: "commands"
    ehs <- v .: "events"

    cmds' <- case CommandTable.resolve addrs cmds of
      Left e -> fail e
      Right a -> return a

    -- FIXME: Validate command names...

    ehs' <- forM ehs $ \eh ->
      case EventHandler.resolve addrs eh of
        Left e -> fail e
        Right a -> return a

    Config <$> ((v .: "config") >>= (.: "device"))
      <*> pure (cConnectionRetryTimeout defaultConfig)
      <*> pure addrs
      <*> pure cmds'
      <*> pure ehs'
  parseJSON invalid = typeMismatch "configuration hash" invalid

defaultConfig :: Config
defaultConfig =
  Config
    { cDevice = "/dev/ttyUSB0",
      cConnectionRetryTimeout = 5,
      cNodeTable = NodeTable.defaultNodeTable,
      cCommandTable = CommandTable.defaultCommandTable,
      cEventHandlers = []
    }

readConfigFile :: FilePath -> IO (Either String Config)
readConfigFile path = do
  result <- YAML.decodeFileEither path

  return $ case result of
    Left e -> Left (show e)
    Right a -> Right a
