module Pcap where

import Control.Applicative

import Data.ByteString as BS (ByteString)
import Data.ByteString.Lazy as LBS (ByteString, fromStrict, empty)

import Data.Binary
import Data.Binary.Get

import Text.Bytedump

import Network.Pcap (PktHdr(..), Link(..), PcapHandle, nextBS)

import qualified Ethernet
import qualified IP4
import qualified UDP

class Pretty a where
    pretty :: a -> String

data Pcap = Pcap
    { _pcapHdr      :: !PktHdr
    , _datalink     :: !Datalink
    , _network      :: !Network
    , _transport    :: !Transport
    , _payload      :: LBS.ByteString
    }
    deriving (Show)

instance Pretty Pcap where
    pretty (Pcap hdr dl net trans pload) = unlines [show hdr, pretty dl, pretty net, pretty trans, pretty pload]

data Datalink = Ethernet !Ethernet.EtherFrame
              | UnsupportedDatalink !Link LBS.ByteString
    deriving (Show)

instance Pretty Datalink where
    pretty (Ethernet frame) = show frame
    pretty (UnsupportedDatalink link raw) = "UnsupportedDatalink " ++ show link ++ "\n" ++ pretty raw

data Network = IP4 IP4.Packet
             | UnsupportedNetwork !Integer LBS.ByteString
    deriving (Show)

instance Pretty Network where
    pretty (IP4 p) = show p
    pretty (UnsupportedNetwork net raw) = "UnsupportedNetwork " ++ show net ++ "\n" ++ pretty raw

data Transport = UDP UDP.Packet
               | UnsupportedTransport !Word8 LBS.ByteString
    deriving (Show)

instance Pretty Transport where
    pretty (UDP p) = show p
    pretty (UnsupportedTransport t raw) = "UnsupportedTransport " ++ show t ++ "\n" ++ pretty raw

instance Pretty BS.ByteString where
    pretty = dumpBS

instance Pretty LBS.ByteString where
    pretty = dumpLBS

nextPcap :: PcapHandle -> Link -> IO (Maybe Pcap)
nextPcap h link = do
    (p, q) <- nextBS h
    if hdrWireLength p == 0
    then
        return Nothing
    else
        return $! Just . runGet (pcap p link) . fromStrict $ q

pcap :: PktHdr -> Link -> Get Pcap
pcap hdr link = do
    dl    <- datalink link
    net   <- network dl
    trans <- transport net
    pload <- getRemainingLazyByteString
    return $! Pcap hdr dl net trans pload

datalink :: Link -> Get Datalink
datalink DLT_EN10MB = ethernet
datalink link       = UnsupportedDatalink <$> pure link <*> getRemainingLazyByteString

ethernet :: Get Datalink
ethernet = Ethernet <$> get

network :: Datalink -> Get Network
network (Ethernet frame) =
    case Ethernet._ethertype frame of
        Ethernet.IP4 -> ip4
        ftype -> UnsupportedNetwork <$> (pure . fromIntegral . Ethernet.fromEtherType) ftype <*> getRemainingLazyByteString
network UnsupportedDatalink{} = return $! UnsupportedNetwork 0 LBS.empty

ip4 :: Get Network
ip4 = IP4 <$> get

transport :: Network -> Get Transport
transport (IP4 p) =
    case IP4._protocol p of
        IP4.UDP -> udp
        prot -> UnsupportedTransport <$> (pure . fromIntegral . IP4.fromProtocol) prot <*> getRemainingLazyByteString
transport UnsupportedNetwork{} = return $! UnsupportedTransport 0 LBS.empty

udp :: Get Transport
udp = UDP <$> get
