{-# LANGUAGE OverloadedStrings #-}

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
       , Address (..)
       , ParsedAddress (..)
       , addressFromMAC
       , mkAddress
       , mkMAC
       , frameAddr
       , parseMAC
       , coordinator
       , broadcast
       , genericNetworkAddress
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word
import qualified Network.Protocol.ZigBee.ZNet25 as Z
import Numeric (readHex)
import Text.Parsec hiding ((<|>))
import Text.Parsec.Text

--------------------------------------------------------------------------------
newtype MAC = MAC Z.Address

--------------------------------------------------------------------------------
-- | A type to represent a node's address.
data Address = Network MAC
               -- ^ A specific node on the network identified by its
               -- MAC address (ZigBee serial number).

             | Local
               -- ^ The node that is directly connected to the
               -- computer via a serial/USB cable.

             | Coordinator
               -- ^ The coordinator node on the network.

             | Broadcast
               -- ^ Broadcast to all nodes.

--------------------------------------------------------------------------------
data ParsedAddress = Unresolved Text | Resolved Address

--------------------------------------------------------------------------------
instance FromJSON MAC where
  parseJSON (String t) = case parseMAC t of
    Left e  -> fail e
    Right m -> return m
  parseJSON invalid = typeMismatch "MAC address" invalid

--------------------------------------------------------------------------------
instance FromJSON ParsedAddress where
  parseJSON (String t) = return $ case t of
    "localnode"   -> Resolved Local
    "broadcast"   -> Resolved Broadcast
    "coordinator" -> Resolved Coordinator
    _             -> Unresolved t
  parseJSON invalid = typeMismatch "node address (string)" invalid

--------------------------------------------------------------------------------
addressFromMAC :: MAC -> Z.Address
addressFromMAC (MAC addr) = addr

--------------------------------------------------------------------------------
mkAddress :: Z.Address -> Address
mkAddress = Network . MAC

--------------------------------------------------------------------------------
-- | Turn an address into a frame address.
frameAddr :: Address -> Z.Address
frameAddr (Network mac) = addressFromMAC mac
frameAddr Local         = addressFromMAC coordinator -- FIXME:
frameAddr Coordinator   = addressFromMAC coordinator
frameAddr Broadcast     = addressFromMAC broadcast

--------------------------------------------------------------------------------
-- | The incoming list MUST have exactly eight (8) elements or error
-- will be called by the Z.address function.
unsafeMkMAC :: [Word8] -> MAC
unsafeMkMAC = MAC . Z.address . ByteString.pack

--------------------------------------------------------------------------------
mkMAC :: [Word8] -> Maybe MAC
mkMAC ws | length ws == 8 = Just (unsafeMkMAC ws)
         | otherwise      = Nothing

--------------------------------------------------------------------------------
coordinator :: MAC
coordinator = unsafeMkMAC [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]

--------------------------------------------------------------------------------
broadcast :: MAC
broadcast = unsafeMkMAC [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff]

--------------------------------------------------------------------------------
genericNetworkAddress :: Z.NetworkAddress
genericNetworkAddress = Z.networkAddress (ByteString.pack [0xff, 0xfe])

--------------------------------------------------------------------------------
parseMAC :: Text -> Either String MAC
parseMAC t =
  case parse macParser (Text.unpack t) t of
    Left pe -> Left (show pe)
    Right m -> Right m

  where
    macParser :: Parser MAC
    macParser = unsafeMkMAC <$> macWords <* eof

    macWords :: Parser [Word8]
    macWords = count 8 parseMACWord

--------------------------------------------------------------------------------
parseMACWord :: Parser Word8
parseMACWord = do
  high <- hexDigit
  low  <- hexDigit
  skipMany (oneOf ".:-")
  skipMany space

  -- This really shouldn't be able to fail.
  case readHex [high, low] of
    [(x, _)] -> return x
    _        -> fail "invalid MAC address"
