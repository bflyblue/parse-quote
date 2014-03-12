module Transport where

import Control.Applicative (pure, (<$>), (<*>))

import Data.Binary
import Data.Binary.Get
import Data.ByteString.Lazy (ByteString, empty)

import Network
import IP4
import UDP

data Transport = UDP UDP.Packet
               | UnsupportedTransport !Word8 ByteString
    deriving (Show)

transport :: Network -> Get Transport
transport (IP4 p) =
    case IP4._protocol p of
        IP4.UDP -> udp
        prot -> UnsupportedTransport <$> (pure . fromIntegral . IP4.fromProtocol) prot <*> getRemainingLazyByteString
transport UnsupportedNetwork{} = return $! UnsupportedTransport 0 empty

udp :: Get Transport
udp = Transport.UDP <$> get
