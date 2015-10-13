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
       , debug
       , runCommander
       , MonadIO
       , liftIO
       , ask
       , asks
       , get
       , gets
       , put
       , modify
       , modify'
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Control.Monad.RWS
import Control.Monad.Trans.Either
import Data.Text (Text)
import qualified Data.Text.IO as Text
import System.IO

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Config
import Network.XXX.ZigBee.Commander.Internal.State

--------------------------------------------------------------------------------
newtype Commander m a =
  Commander {unC :: RWST Config () State (EitherT String m) a}
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadReader Config, MonadState State
           )

--------------------------------------------------------------------------------
-- FIXME:
logger :: (MonadIO m) => Text -> Commander m ()
logger = liftIO . Text.hPutStrLn stderr

--------------------------------------------------------------------------------
-- FIXME:
debug :: (Monad m) => Commander m () -> Commander m ()
debug = when True

--------------------------------------------------------------------------------
runCommander :: (Monad m)
             => Config
             -> Commander m a
             -> m (Either String a)
runCommander config cmdr = do
  result <- runEitherT $ evalRWST (unC cmdr) config
                                  (initialState $ cDeviceFile config)

  return $ case result of
    Left e       -> Left  e
    Right (a, _) -> Right a
