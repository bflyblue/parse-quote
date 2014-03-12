module Pcap where

import Data.ByteString as BS (ByteString)
import Data.ByteString.Lazy as LBS (ByteString, fromStrict)

import Data.Binary
import Data.Binary.Get

import Text.Bytedump

import Network.Pcap (PktHdr(..), Link(..), PcapHandle, nextBS)

import Datalink
import Network
import Transport

data Pcap = Pcap
    { _pcapHdr      :: !PktHdr
    , _datalink     :: !Datalink
    , _network      :: !Network
    , _transport    :: !Transport
    , _payload      :: LBS.ByteString
    }
    deriving (Show)

-- |'nextPcap' reads the next packet from 'PcapHandle' if there is more data.
nextPcap :: PcapHandle -> Link -> IO (Maybe Pcap)
nextPcap h link = do
    (p, q) <- nextBS h
    if hdrWireLength p == 0
    then
        return Nothing
    else
        return $! Just . runGet (pcap p link) . fromStrict $ q

-- |Decode a pcap packet.
pcap :: PktHdr -> Link -> Get Pcap
pcap hdr link = do
    dl    <- datalink link
    net   <- network dl
    trans <- transport net
    pload <- getRemainingLazyByteString
    return $! Pcap hdr dl net trans pload

-- Useful for dumping raw packets for debugging

class Pretty a where
    pretty :: a -> String

instance Pretty Pcap where
    pretty (Pcap hdr dl net trans pload) = unlines [show hdr, pretty dl, pretty net, pretty trans, pretty pload]

instance Pretty Datalink where
    pretty (Ethernet frame) = show frame
    pretty (UnsupportedDatalink link raw) = "UnsupportedDatalink " ++ show link ++ "\n" ++ pretty raw

instance Pretty Network where
    pretty (IP4 p) = show p
    pretty (UnsupportedNetwork net raw) = "UnsupportedNetwork " ++ show net ++ "\n" ++ pretty raw

instance Pretty Transport where
    pretty (UDP p) = show p
    pretty (UnsupportedTransport t raw) = "UnsupportedTransport " ++ show t ++ "\n" ++ pretty raw

instance Pretty BS.ByteString where
    pretty = dumpBS

instance Pretty LBS.ByteString where
    pretty = dumpLBS
