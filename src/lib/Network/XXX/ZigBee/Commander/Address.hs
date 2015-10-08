{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Network.XXX.ZigBee.Commander.Address
       ( MAC
       , addressFromMAC
       , coordinator
       , broadcast
       , genericNetworkAddress
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import qualified Data.ByteString as ByteString
import qualified Network.Protocol.ZigBee.ZNet25 as Z

--------------------------------------------------------------------------------
data MAC = MAC Z.Address

--------------------------------------------------------------------------------
addressFromMAC :: MAC -> Z.Address
addressFromMAC (MAC addr) = addr

--------------------------------------------------------------------------------
coordinator :: MAC
coordinator = undefined

--------------------------------------------------------------------------------
broadcast :: MAC
broadcast = undefined

--------------------------------------------------------------------------------
genericNetworkAddress :: Z.NetworkAddress
genericNetworkAddress = Z.networkAddress (ByteString.pack [0xff, 0xfe])
