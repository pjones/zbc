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
module Network.XXX.ZigBee.Commander.Command
       ( Command (..)
       , mkFrame
       , ATCode
       , mkATCode
       , Payload
       , payload
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)

--------------------------------------------------------------------------------
-- Package Imports:
import qualified Network.Protocol.ZigBee.ZNet25 as Z

--------------------------------------------------------------------------------
-- Local Imports:
import Network.XXX.ZigBee.Commander.Address

--------------------------------------------------------------------------------
-- | Commands that can be sent to remote devices.
data Command = AT ATCode (Maybe Payload)

--------------------------------------------------------------------------------
-- | An @ATCode@ is a two-byte code.
newtype ATCode = ATCode (Word8, Word8)

--------------------------------------------------------------------------------
-- | Some commands require a payload to be sent with them.
data Payload = Payload ByteString

--------------------------------------------------------------------------------
instance FromJSON Command where
  parseJSON (Object v) = do
    typeName <- v .: "type"

    case typeName of
      "AT" -> AT <$> v .: "code" <*> v .: "payload"
      _    -> fail ("unknown command type: " ++ Text.unpack typeName)
  parseJSON invalid = typeMismatch "command" invalid

--------------------------------------------------------------------------------
instance FromJSON ATCode where
  parseJSON (String t) = case parseATCode t of
    Just c  -> return c
    Nothing -> fail ("invalid AT code: " ++ Text.unpack t)
  parseJSON invalid = typeMismatch "AT code" invalid

--------------------------------------------------------------------------------
instance FromJSON Payload where
  parseJSON (String t) = return $ Payload (Text.encodeUtf8 t)
  parseJSON invalid = typeMismatch "AT command payload" invalid

--------------------------------------------------------------------------------
-- | Convert user generated text into a valid 'ATCode' (or not).
parseATCode :: Text -> Maybe ATCode
parseATCode = fromBS . Text.encodeUtf8
  where
    fromBS :: ByteString -> Maybe ATCode
    fromBS bs = if ByteString.length bs == 2
                  then Just $ ATCode (ByteString.head bs, ByteString.last bs)
                  else Nothing

--------------------------------------------------------------------------------
mkATCode :: (Word8, Word8) -> ATCode
mkATCode = ATCode

--------------------------------------------------------------------------------
unatcode :: ATCode -> Z.CommandName
unatcode (ATCode (x, y)) = Z.commandName $ map (toEnum . fromEnum) [x, y]

--------------------------------------------------------------------------------
-- | Turn a 'ByteString' into a 'Payload'.
payload :: ByteString -> Maybe Payload
payload bs | ByteString.null bs = Nothing
           | otherwise          = Just (Payload bs)

--------------------------------------------------------------------------------
unpayload :: Maybe Payload -> ByteString
unpayload Nothing             = ByteString.empty
unpayload (Just (Payload bs)) = bs

--------------------------------------------------------------------------------
mkFrame :: Z.FrameId -> Address -> Command -> Z.Frame
mkFrame fid dest cmd =
  case (dest, cmd) of
    -- Local AT Command:
    (Local, AT code params) ->
      Z.ATCommand fid (unatcode code) (unpayload params)

    -- Remote AT Command:
    (_, AT code params) ->
      Z.RemoteCommandRequest fid (frameAddr dest) genericNetworkAddress
                             0x02 (unatcode code) (unpayload params)
