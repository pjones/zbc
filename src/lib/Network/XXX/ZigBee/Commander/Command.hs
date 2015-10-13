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
       , Destination (..)
       , ATCode
       , atCode
       , Payload
       , payload
       ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
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
data Command = AT Destination ATCode (Maybe Payload)

--------------------------------------------------------------------------------
-- | Where to send commands.
data Destination = ToNode MAC    -- ^ Send to a specific node.
                 | ToLocal       -- ^ Send to the locally connected node.
                 | ToCoordinator -- ^ Send to the local coordinator.
                 | Broadcast     -- ^ Broadcast to all nodes.

--------------------------------------------------------------------------------
-- | Turn a destination address into a ZigBee address.
undest :: Destination -> Z.Address
undest (ToNode mac)  = addressFromMAC mac
undest ToLocal       = addressFromMAC coordinator
undest ToCoordinator = addressFromMAC coordinator
undest Broadcast     = addressFromMAC broadcast

--------------------------------------------------------------------------------
-- | An @ATCode@ is a two-byte code.
type ATCode = (Word8, Word8)

--------------------------------------------------------------------------------
-- | Convert user generated text into a valid 'ATCode' (or not).
atCode :: Text -> Maybe ATCode
atCode = fromBS . Text.encodeUtf8
  where
    fromBS :: ByteString -> Maybe ATCode
    fromBS bs = if ByteString.length bs == 2
                  then Just (ByteString.head bs, ByteString.last bs)
                  else Nothing

--------------------------------------------------------------------------------
unatcode :: ATCode -> Z.CommandName
unatcode (x, y) = Z.commandName $ map (toEnum . fromEnum) [x, y]

--------------------------------------------------------------------------------
-- | Some commands require a payload to be sent with them.
data Payload = Payload ByteString

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
mkFrame :: Z.FrameId -> Command -> Z.Frame
mkFrame fid cmd =
  case cmd of
    -- Local AT Command:
    AT ToLocal code params ->
      Z.ATCommand fid (unatcode code) (unpayload params)

    -- Remote AT Command:
    AT dest code params ->
      Z.RemoteCommandRequest fid (undest dest) genericNetworkAddress 0x02
                             (unatcode code) (unpayload params)
