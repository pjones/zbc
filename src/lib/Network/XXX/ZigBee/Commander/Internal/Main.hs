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
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (forever)
import qualified Network.Protocol.ZigBee.ZNet25 as Z
import System.IO

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Config
import Network.XXX.ZigBee.Commander.Internal.Commander
import Network.XXX.ZigBee.Commander.Internal.Serial

--------------------------------------------------------------------------------
commanderMain :: IO ()
commanderMain = do
  inchan  <- newChan
  outchan <- newChan

  serialIOThread <- async (serialIO inchan outchan)

  _ <- forever $ do
    incoming <- readChan outchan
    hPrint stderr incoming

  cancel serialIOThread

  where
    serialIO :: Chan Z.Frame -> Chan Z.Frame -> IO ()
    serialIO inchan outchan = do
      _ <- runCommander defaultConfig $ forever (serialThread inchan outchan)
      return ()
