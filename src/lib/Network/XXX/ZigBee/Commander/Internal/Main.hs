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
import System.Environment

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Config
import Network.XXX.ZigBee.Commander.Internal.Commander
import Network.XXX.ZigBee.Commander.Internal.Dispatch
import Network.XXX.ZigBee.Commander.Internal.Environment
import Network.XXX.ZigBee.Commander.Internal.Serial

--------------------------------------------------------------------------------
commanderMain :: IO ()
commanderMain = do
  (x:_)   <- getArgs
  configM <- readConfigFile x

  env <- case configM of
           Left e  -> fail e
           Right a -> newEnvironment a

  serialIOThread <- async (void $ runCommander env serialThread)
  dispatchThread <- async (void $ runCommander env dispatch)

  wait dispatchThread
  cancel serialIOThread
