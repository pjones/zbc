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
       , SkipType (..)
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
import qualified Data.Aeson.Types as Aeson
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char (toLower)
import Data.Functor.Identity
import Data.Maybe (maybeToList, catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8, Word16)
import qualified Network.Protocol.ZigBee.ZNet25 as Z
import Text.Parsec hiding ((<|>))
import Text.Parsec.Text

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Address
import Network.XXX.ZigBee.Commander.GPIO
import Network.XXX.ZigBee.Commander.Internal.Resolve
import Network.XXX.ZigBee.Commander.Node
import Network.XXX.ZigBee.Commander.NodeTable (NodeTable)
import qualified Network.XXX.ZigBee.Commander.NodeTable as NodeTable

--------------------------------------------------------------------------------
data EventType = NodeIdentification
                 -- ^ A node has joined the network or responded to
                 -- a discovery request.

               | DigitalSampleIndicator
                 -- ^ A node has sent a digital sample.

               deriving (Show, Eq)

--------------------------------------------------------------------------------
data EventMatcher = MatchDigitalLow PinID
                  | MatchDigitalHigh PinID
                  deriving (Eq, Show)

--------------------------------------------------------------------------------
data SkipType = SkipMuted

--------------------------------------------------------------------------------
data EventAction = SendCommand Text
                 | Wait Int
                 | Mute Int
                 | Skip SkipType

--------------------------------------------------------------------------------
data EventHandler' a = EventHandler
  { eventType    :: EventType
  , eventNode    :: a Address
  , eventMatcher :: Maybe EventMatcher
  , eventActions :: [EventAction]
  }

--------------------------------------------------------------------------------
type EventHandler = EventHandler' Identity

--------------------------------------------------------------------------------
data Event = JoinNotification Address NodeName NodeType
           | DiscoveryNotification Address NodeName NodeType
           | DigitalSample Address PinID DigitalState
           deriving (Show)

--------------------------------------------------------------------------------
instance FromJSON EventType where
  parseJSON (String t) = case Text.toLower t of
    "node identification" -> return NodeIdentification
    "identification"      -> return NodeIdentification
    "digital sample"      -> return DigitalSampleIndicator
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
  parseJSON (Object v) = do
    etype <- v .: "when"

    EventHandler <$> pure etype
                 <*> (parseUnresolved =<< (v .: "node"))
                 <*> (parseMatcher etype =<< (v .:? "with"))
                 <*> v .: "actions"
  parseJSON invalid = typeMismatch "event handler" invalid

--------------------------------------------------------------------------------
parseEventAction :: Text -> Either String EventAction
parseEventAction t =
  case parse paction (Text.unpack t) t of
    Left e  -> Left (show e)
    Right a -> Right a

  where
    paction :: Parser EventAction
    paction = choice [try psend, try pwait, try pmute, pskip] <* eof

    psend :: Parser EventAction
    psend = do
      void (string "send")
      skipMany space
      SendCommand . Text.pack <$> many1 anyChar

    pwait :: Parser EventAction
    pwait = do
      void (string "wait")
      skipMany space
      Wait . read <$> many1 digit

    pmute :: Parser EventAction
    pmute = do
      void (string "mute")
      skipMany space
      Mute . read <$> many1 digit

    pskip :: Parser EventAction
    pskip = do
      void (string "skip")
      skipMany space
      void (string "muted")
      return (Skip SkipMuted)

--------------------------------------------------------------------------------
parseMatcher :: EventType -> Maybe Aeson.Value -> Aeson.Parser (Maybe EventMatcher)
parseMatcher _ Nothing = return Nothing
parseMatcher etype (Just (String t)) = case etype of
  NodeIdentification ->
    fail "`with' is not a valid key when using `Node Identification'"

  DigitalSampleIndicator ->
    case parseDigitalSampleIndicator t of
      Left e  -> fail e
      Right a -> return (Just a)

parseMatcher _ (Just a) = typeMismatch "event `with' (string)" a

--------------------------------------------------------------------------------
parseDigitalSampleIndicator :: Text -> Either String EventMatcher
parseDigitalSampleIndicator t =
  case parse pmatcher (Text.unpack t) t of
    Left e  -> Left (show e)
    Right a -> Right a

  where
    pmatcher :: Parser EventMatcher
    pmatcher = do
      pin     <- skipMany letter *> many1 digit
      matcher <- skipMany space  *> pstate (read pin)
      eof
      return matcher

    pstate :: PinID -> Parser EventMatcher
    pstate pin = do
      state <- many1 letter

      case map toLower state of
        "low"  -> return (MatchDigitalLow  pin)
        "high" -> return (MatchDigitalHigh pin)
        _      -> fail ("unexpected `" ++ state ++ "' expected low or high")

--------------------------------------------------------------------------------
resolve :: NodeTable
        -> EventHandler' Unresolved
        -> Either String EventHandler
resolve nodes EventHandler {..} =
  EventHandler <$> pure eventType
               <*> (Identity <$> NodeTable.resolve nodes eventNode)
               <*> pure eventMatcher
               <*> pure eventActions

--------------------------------------------------------------------------------
-- | Gather basic details about an event.
eventDetails :: Event -> (Address, EventType)
eventDetails (JoinNotification a _ _)      = (a, NodeIdentification)
eventDetails (DiscoveryNotification a _ _) = (a, NodeIdentification)
eventDetails (DigitalSample a _ _)         = (a, DigitalSampleIndicator)

--------------------------------------------------------------------------------
-- | Filter the list of event handlers so it only contains those that
-- match the specified event.
eventHandlers :: Event -> [EventHandler] -> [EventHandler]
eventHandlers event = filter (go $ eventDetails event)
  where
    go :: (Address, EventType) -> EventHandler -> Bool
    go (addr, etype) (EventHandler {..}) =
      eventType             == etype &&
      runIdentity eventNode == addr  &&
      checkMatcher eventMatcher

    checkMatcher :: Maybe EventMatcher -> Bool
    checkMatcher Nothing   = True -- No matcher means match all.
    checkMatcher (Just em) = case event of
      DigitalSample _ pin DigitalLow  -> em == MatchDigitalLow  pin
      DigitalSample _ pin DigitalHigh -> em == MatchDigitalHigh pin
      _                               -> False

--------------------------------------------------------------------------------
frameToEvent :: Z.Frame -> [Event]
frameToEvent frame = case frame of
  Z.ATCommandResponse _ name status response ->
    eventFromATResponse (Z.unCommandName name) status response

  Z.NodeIdentificationIndicator addr _ _ _ _ name _ dt _ _ _ ->
    [JoinNotification (mkAddress addr) (Text.pack name)
                      (nodeTypeFromDeviceType dt)]

  Z.ZigBeeIODataSampleIndicator addr _ _ _ dmask _ payload ->
    eventsFromDigitalSample (mkAddress addr) dmask payload

  _ -> []

--------------------------------------------------------------------------------
eventFromATResponse :: String -> Word8 -> ByteString -> [Event]
eventFromATResponse "ND" 0 bs = maybeToList (parseDiscoveryNotification bs)
eventFromATResponse _ _ _     = []

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
    addr          = ByteString.take 8 (ByteString.drop 2 bs)
    (ni, afterni) = ByteString.break (== 0) (ByteString.drop 10 bs)
    dt            = ByteString.take 1 (ByteString.drop 2 afterni)
    nwaddr        = Network <$> mkMAC (ByteString.unpack addr)
    name          = Just (Text.decodeUtf8 ni)
    nt            = if ByteString.length dt == 1
                      then Just (nodeTypeFromDeviceType (ByteString.head dt))
                      else Nothing

--------------------------------------------------------------------------------
-- | Given frame information about a digital sample, create events
-- for all digital pins that were in the sample.
eventsFromDigitalSample :: Address
                        -> Z.DigitalChannelMask
                        -> ByteString
                        -> [Event]
eventsFromDigitalSample addr mask payload =
    catMaybes [bitToEvent (pred x) | x <- [1 .. finiteBitSize mask]]

  where
    -- The first two bytes in the payload are a state mask that tells
    -- you the state of the sampled digital pins.
    state :: Maybe Word16
    state = case ByteString.unpack payload of
      x:y:_ -> Just (mkWord16 x y)
      _     -> Nothing

    -- Join the first two bytes of the sample into a 16-bit word.
    mkWord16 :: Word8 -> Word8 -> Word16
    mkWord16 x y = let x' = fromIntegral x :: Word16
                       y' = fromIntegral y :: Word16
                   in shift x' 8 .|. y'

    -- Try to create an event.
    bitToEvent :: Int -> Maybe Event
    bitToEvent pin =
      if testBit mask pin
        then DigitalSample addr pin . toEnum . fromEnum . flip testBit pin <$> state
        else Nothing
