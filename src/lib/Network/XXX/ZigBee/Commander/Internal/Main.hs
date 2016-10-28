{-# LANGUAGE RecordWildCards #-}

{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Network.XXX.ZigBee.Commander.Internal.Main
       ( commanderMain
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Control.Concurrent.Async
import Control.Monad (void)
import Options.Applicative
import System.Exit

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Config
import Network.XXX.ZigBee.Commander.Internal.Commander
import Network.XXX.ZigBee.Commander.Internal.Dispatch
import Network.XXX.ZigBee.Commander.Internal.Environment
import qualified Network.XXX.ZigBee.Commander.Internal.Network as Network
import Network.XXX.ZigBee.Commander.Internal.Serial

--------------------------------------------------------------------------------
data RunMode = VersionMode
             | ServerMode ServerOptions
             | ClientMode ClientOptions

--------------------------------------------------------------------------------
data ServerOptions = ServerOptions
  { configFile :: FilePath
  }

--------------------------------------------------------------------------------
data ClientOptions = ClientOptions
  { request :: String
  }

--------------------------------------------------------------------------------
optionParser :: Parser RunMode
optionParser =
    flag' VersionMode (long "version" <> help "Show version and exit") <|>
      subparser (serverMode <> clientMode)
  where
    serverMode = command "server"
                         (info (helper <*> serverOptions)
                          (progDesc "Manage a network of devices"))

    clientMode = command "client"
                         (info (helper <*> clientOptions)
                          (progDesc "Connect to a running server"))

    serverOptions = (ServerMode . ServerOptions)
      <$> strOption (short 'c'      <>
                     long "config"  <>
                     metavar "FILE" <>
                     help "Configuration file")

    clientOptions = (ClientMode . ClientOptions)
      <$> strOption (short 'e'      <>
                     long "eval"    <>
                     metavar "CMD"  <>
                     help "Send a single request to the server")

--------------------------------------------------------------------------------
commanderMain :: IO ()
commanderMain = do
  runMode <- execParser (info (helper <*> optionParser) fullDesc)

  case runMode of
    ServerMode ops -> serverCommand ops
    ClientMode ops -> clientCommand ops
    VersionMode    -> putStrLn "version: xxx"

  exitSuccess

--------------------------------------------------------------------------------
serverCommand :: ServerOptions -> IO ()
serverCommand ServerOptions{..} = do
  configM <- readConfigFile configFile

  env <- case configM of
           Left e  -> fail e
           Right a -> newEnvironment a

  serialIOThread <- async (void $ runCommander env serialThread)
  serverThread   <- async (void $ runCommander env Network.server)
  dispatchThread <- async (void $ runCommander env dispatch)

  wait dispatchThread
  cancel serverThread
  cancel serialIOThread

--------------------------------------------------------------------------------
clientCommand :: ClientOptions -> IO ()
clientCommand ClientOptions {..} = do
  rr <- case Network.parseRemoteRequest request of
          Nothing     -> fail ("invalid request: " ++ request)
          Just parsed -> return parsed

  Network.client (`Network.send` rr)
