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
import Control.Monad (forever, void)
import qualified Network.Protocol.ZigBee.ZNet25 as Z
import System.IO

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Config
import Network.XXX.ZigBee.Commander.Internal.Commander
import Network.XXX.ZigBee.Commander.Internal.Dispatch
import Network.XXX.ZigBee.Commander.Internal.Serial

--------------------------------------------------------------------------------
commanderMain :: IO ()
commanderMain = do
  inchan  <- newChan
  outchan <- newChan
  let config = defaultConfig

  serialIOThread <- async (serialIO config inchan outchan)
  dispatchThread <- async (dispatchLoop config inchan outchan)

  wait dispatchThread
  cancel serialIOThread

  where
    serialIO :: Config -> Chan Z.Frame -> Chan Z.Frame -> IO ()
    serialIO config inchan outchan =
      void $ runCommander config $ forever (serialThread inchan outchan)

    dispatchLoop :: Config -> Chan Z.Frame -> Chan Z.Frame -> IO ()
    dispatchLoop config inchan outchan = void $ runCommander config $ forever $ do
      incoming <- liftIO (readChan outchan)
      liftIO (hPrint stderr incoming) -- FIXME:
      outgoing <- dispatch incoming
      liftIO (writeList2Chan inchan outgoing)
