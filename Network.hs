module Network where

import Control.Applicative (pure, (<$>), (<*>))

import Data.Binary
import Data.Binary.Get
import Data.ByteString.Lazy (ByteString, empty)

import Ethernet (EtherFrame(..), EtherType(..), fromEtherType)
import Datalink
import IP4

data Network = IP4 Packet
             | UnsupportedNetwork !Integer ByteString
    deriving (Show)

network :: Datalink -> Get Network
network (Ethernet frame) =
    case _ethertype frame of
        Ethernet.IP4 -> ip4
        ftype -> UnsupportedNetwork <$> (pure . fromIntegral . fromEtherType) ftype <*> getRemainingLazyByteString
network UnsupportedDatalink{} = return $! UnsupportedNetwork 0 empty

ip4 :: Get Network
ip4 = Network.IP4 <$> get
