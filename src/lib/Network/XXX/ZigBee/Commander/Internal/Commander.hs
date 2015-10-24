{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Network.XXX.ZigBee.Commander.Internal.Commander
       ( Commander
       , logger
       , loggerS
       , debug
       , nextFrameID
       , spawn
       , runCommander
       , MonadIO
       , liftIO
       , ask
       , asks
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Network.Protocol.ZigBee.ZNet25 as Z
import System.IO

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Internal.Environment
import Network.XXX.ZigBee.Commander.Internal.State

--------------------------------------------------------------------------------
newtype Commander m a =
  Commander {unC :: ReaderT Environment (EitherT String m) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Environment)

--------------------------------------------------------------------------------
-- FIXME:
logger :: (MonadIO m) => Text -> Commander m ()
logger = liftIO . Text.hPutStrLn stderr

--------------------------------------------------------------------------------
loggerS :: (MonadIO m) => String -> Commander m ()
loggerS = logger . Text.pack

--------------------------------------------------------------------------------
-- FIXME:
debug :: (Monad m) => Commander m () -> Commander m ()
debug = when True

--------------------------------------------------------------------------------
nextFrameID :: (MonadIO m) => Commander m Z.FrameId
nextFrameID = do
  stateVar <- asks state

  liftIO . atomically $ do
    s <- readTVar stateVar

    let fid = if frameID s == maxBound
                then 1
                else frameID s + 1

    writeTVar stateVar s {frameID = fid}
    return fid

--------------------------------------------------------------------------------
spawn :: Commander IO a -> Commander IO (Async (Either String a))
spawn action = liftIO . async . flip runCommander action =<< ask

--------------------------------------------------------------------------------
runCommander :: (Monad m)
             => Environment
             -> Commander m a
             -> m (Either String a)
runCommander env cmdr = do
  result <- runEitherT $ runReaderT (unC cmdr) env

  return $ case result of
    Left  e -> Left  e
    Right a -> Right a
