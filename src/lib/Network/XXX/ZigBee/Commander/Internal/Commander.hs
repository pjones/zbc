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
       , runCommander
       , MonadIO
       , liftIO
       , ask
       , asks
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Control.Monad.Reader
import Control.Monad.Trans.Either

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Config

--------------------------------------------------------------------------------
newtype Commander m a =
  Commander {unC :: ReaderT Config (EitherT String m) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

--------------------------------------------------------------------------------
runCommander :: Config -> Commander m a -> m (Either String a)
runCommander config cmdr = runEitherT $ runReaderT (unC cmdr) config
