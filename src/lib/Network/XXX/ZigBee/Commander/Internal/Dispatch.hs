{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

module Network.XXX.ZigBee.Commander.Internal.Dispatch
  ( dispatch,
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (forever, void, when)
import Data.Monoid
import qualified Data.Text as Text
import Network.XXX.ZigBee.Commander.Config
import Network.XXX.ZigBee.Commander.Event
import Network.XXX.ZigBee.Commander.Internal.Commander
import Network.XXX.ZigBee.Commander.Internal.Environment
import qualified Network.XXX.ZigBee.Commander.Internal.Ops as Ops
import qualified System.Process as Process

dispatch :: Commander IO ()
dispatch = forever go
  where
    go :: Commander IO ()
    go = do
      event <- asks events >>= liftIO . readChan
      void $ spawn (handleEvent event)

    handleEvent :: Event -> Commander IO ()
    handleEvent event = do
      as <- concatMap eventActions <$> handlers event
      runWhileTrue event as

    runWhileTrue :: Event -> [EventAction] -> Commander IO ()
    runWhileTrue _ [] = return ()
    runWhileTrue event (a : as) = do
      bool <- action event a
      when bool (runWhileTrue event as)

    handlers :: (Monad m) => Event -> Commander m [EventHandler]
    handlers event = eventHandlers event . cEventHandlers <$> asks config

    action :: (MonadIO m) => Event -> EventAction -> Commander m Bool
    action event ea = case ea of
      SendCommand name -> do
        Ops.send name
        return True
      ShellCommandAsync command -> do
        debug (logger $ "running async shell command: " <> command)
        liftIO . void . async . Process.callCommand . Text.unpack $ command
        return True
      Wait delay -> do
        debug (loggerS $ "waiting: " ++ show delay)
        liftIO (threadDelay $ delay * 1000)
        return True
      Mute delay -> do
        -- Mute a node for the given number of milliseconds.
        mute (fst $ eventDetails event) delay
        return True
      Skip SkipMuted ->
        not <$> isMuted (fst $ eventDetails event)
