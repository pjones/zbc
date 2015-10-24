{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

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
       , EventHandler' (..)
       , EventHandler
       , Event (..)
       , resolve
       , eventDetails
       , eventHandlers
       , frameToEvent
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)
import qualified Network.Protocol.ZigBee.ZNet25 as Z
import Text.Parsec hiding ((<|>))
import Text.Parsec.Text

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Address
import Network.XXX.ZigBee.Commander.Internal.Resolve
import Network.XXX.ZigBee.Commander.Node
import Network.XXX.ZigBee.Commander.NodeTable (NodeTable)
import qualified Network.XXX.ZigBee.Commander.NodeTable as NodeTable

--------------------------------------------------------------------------------
data EventType = NodeIdentification
                 -- ^ A node has joined the network or responded to
                 -- a discovery request.

               deriving (Show, Eq)

--------------------------------------------------------------------------------
data EventAction = SendCommand Text

--------------------------------------------------------------------------------
data EventHandler' a = EventHandler
  { eventType    :: EventType
  , eventNode    :: a Address
  , eventActions :: [EventAction]
  }

--------------------------------------------------------------------------------
type EventHandler = EventHandler' Identity

--------------------------------------------------------------------------------
data Event = JoinNotification Address NodeName NodeType
           | DiscoveryNotification Address NodeName NodeType
           deriving (Show)

--------------------------------------------------------------------------------
instance FromJSON EventType where
  parseJSON (String t) = case Text.toLower t of
    "node identification" -> return NodeIdentification
    "identification"      -> return NodeIdentification
    _                     -> fail ("invalid event name: " ++ Text.unpack t)
  parseJSON invalid = typeMismatch "event name" invalid

--------------------------------------------------------------------------------
instance FromJSON EventAction where
  parseJSON (String t) =
    case parseEventAction t of
      Left e  -> fail e
      Right a -> return a
  parseJSON invalid = typeMismatch "event action" invalid

--------------------------------------------------------------------------------
instance (FromUnresolved a) => FromJSON (EventHandler' a) where
  parseJSON (Object v) =
    EventHandler <$> v .: "when"
                 <*> (parseUnresolved =<< (v .: "node"))
                 <*> v .: "actions"
  parseJSON invalid = typeMismatch "event handler" invalid

--------------------------------------------------------------------------------
parseEventAction :: Text -> Either String EventAction
parseEventAction t =
  case parse psend (Text.unpack t) t of
    Left e  -> Left (show e)
    Right a -> Right a

  where
    psend :: Parser EventAction
    psend = do
      void (string "send")
      skipMany space
      SendCommand . Text.pack <$> many1 anyChar <* eof

--------------------------------------------------------------------------------
resolve :: NodeTable
        -> EventHandler' Unresolved
        -> Either String EventHandler
resolve nodes EventHandler {..} =
  EventHandler <$> pure eventType
               <*> (Identity <$> NodeTable.resolve nodes eventNode)
               <*> pure eventActions

--------------------------------------------------------------------------------
-- | Gather basic details about an event.
eventDetails :: Event -> (Address, EventType)
eventDetails (JoinNotification a _ _)      = (a, NodeIdentification)
eventDetails (DiscoveryNotification a _ _) = (a, NodeIdentification)

--------------------------------------------------------------------------------
-- | Filter the list of event handlers so it only contains those that
-- match the specified event.
eventHandlers :: Event -> [EventHandler] -> [EventHandler]
eventHandlers event = filter (go $ eventDetails event)
  where
    go :: (Address, EventType) -> EventHandler -> Bool
    go (addr, etype) (EventHandler {..}) =
      eventType == etype &&
      runIdentity eventNode == addr

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
