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
module Network.XXX.ZigBee.Commander.Event
       ( EventType (..)
       , EventAction (..)
       , EventHandler (..)
       , Event (..)
       , eventDetails
       , frameToEvent
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)
import qualified Network.Protocol.ZigBee.ZNet25 as Z
import Network.XXX.ZigBee.Commander.Command

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Address
import Network.XXX.ZigBee.Commander.Node

--------------------------------------------------------------------------------
data EventType = NodeIdentification
                 -- ^ A node has joined the network or responded to
                 -- a discovery request.

--------------------------------------------------------------------------------
data EventAction = SendCommand Command

--------------------------------------------------------------------------------
data EventHandler = EventHandler
  { eventType    :: EventType
  , eventNode    :: Address
  , eventActions :: [EventAction]
  }

--------------------------------------------------------------------------------
data Event = JoinNotification Address NodeName NodeType
           | DiscoveryNotification Address NodeName NodeType

--------------------------------------------------------------------------------
-- | Gather basic details about an event.
eventDetails :: Event -> (Address, EventType)
eventDetails (JoinNotification a _ _)      = (a, NodeIdentification)
eventDetails (DiscoveryNotification a _ _) = (a, NodeIdentification)

--------------------------------------------------------------------------------
frameToEvent :: Z.Frame -> Maybe Event
frameToEvent frame = case frame of
  Z.ATCommandResponse _ name status response ->
    eventFromATResponse (Z.unCommandName name) status response

  Z.NodeIdentificationIndicator addr _ _ _ _ name _ dt _ _ _ ->
    Just (JoinNotification (mkAddress addr) (Text.pack name)
                           (nodeTypeFromDeviceType dt))

  _ -> Nothing

--------------------------------------------------------------------------------
eventFromATResponse :: String -> Word8 -> ByteString -> Maybe Event
eventFromATResponse "ND" 0 bs = parseDiscoveryNotification bs
eventFromATResponse _ _ _     = Nothing

--------------------------------------------------------------------------------
-- | Parse the payload of the ATND response frame.
--
-- Payload structure:
--   2 bytes: randomly generated network address.
--   8 bytes: 64-bit MAC address
--   N bytes: NULL-terminated node identification string
--   2 bytes: Parent network address.
--   1  byte: Device type.
--   1  byte: Status.
--   2 bytes: profile ID.
--   2 bytes: manufacture ID.
parseDiscoveryNotification :: ByteString -> Maybe Event
parseDiscoveryNotification bs =
  DiscoveryNotification <$> nwaddr
                        <*> name
                        <*> nt
  where
    addr          = ByteString.take 8 (ByteString.take 2 bs)
    (ni, afterni) = ByteString.break (== 0) (ByteString.drop 10 bs)
    dt            = ByteString.take 1 (ByteString.drop 2 afterni)
    nwaddr        = Network <$> mkMAC (ByteString.unpack addr)
    name          = Just (Text.decodeUtf8 ni)
    nt            = if ByteString.length dt == 1
                      then Just (nodeTypeFromDeviceType (ByteString.head dt))
                      else Nothing
