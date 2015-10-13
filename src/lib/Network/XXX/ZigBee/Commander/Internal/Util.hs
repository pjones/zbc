{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Network.XXX.ZigBee.Commander.Internal.Util
       ( hexdump
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Internal.Commander

--------------------------------------------------------------------------------
hexdump :: (MonadIO m) => Text -> ByteString -> Commander m ()
hexdump msg b = let encoded = concatMap (printf "%02x ") (ByteString.unpack b)
                 in logger (msg <> Text.pack encoded)
