{-# LANGUAGE DeriveGeneric #-}

module IP4 where

import GHC.Generics

import Control.Applicative
import Control.Monad

import Data.Binary
import Data.Binary.Get

import Data.Bits

data Packet = Packet
    { _version              :: !Word8
    , _ihl                  :: !Word8
    , _dscp                 :: !Word8
    , _ecn                  :: !Word8
    , _totalLength          :: !Word16
    , _identification       :: !Word16
    , _flags                :: !Word8
    , _fragmentOffset       :: !Word16
    , _ttl                  :: !Word8
    , _protocol             :: !Protocol
    , _headerChecksum       :: !Word16
    , _sourceAddress        :: !IP4Address
    , _destinationAddress   :: !IP4Address
    , _options              :: [Word32]
    } deriving (Show)

instance Binary Packet where
    get = do
        ver_ihl         <- getWord8
        dscp_ecn        <- getWord8
        totalLength     <- getWord16be
        identification  <- getWord16be
        flags_frag      <- getWord16be
        ttl             <- getWord8
        protocol        <- get
        headercsum      <- getWord16be
        srcaddr         <- get
        dstaddr         <- get
        let ver   = select ver_ihl      4 0xf
            ihl   = select ver_ihl      0 0xf
            dscp  = select dscp_ecn     2 0x6f
            ecn   = select dscp_ecn     0 0x3
            flags = select flags_frag  14 0x7
            frag  = select flags_frag   0 0x1fff
        when (ihl < 5) $ fail "Internet Header Length < 5"
        options         <- replicateM (fromIntegral ihl - 5) getWord32be
        return $! Packet ver ihl dscp ecn totalLength identification (fromIntegral flags) frag ttl protocol headercsum srcaddr dstaddr options
        where
            select v s a = (v `shiftR` s) .&. a

    put = undefined     -- TODO

data IP4Address = IP4Address !Word8 !Word8 !Word8 !Word8
    deriving (Show, Generic)

instance Binary IP4Address

data Protocol = TCP
              | UDP
              | Unknown !Word8
    deriving (Show, Generic)

instance Binary Protocol where
    get = toProtocol <$> getWord8
    put = putWord8 . fromProtocol

toProtocol :: Word8 -> Protocol
toProtocol 6                = TCP
toProtocol 17               = UDP
toProtocol w                = Unknown w

fromProtocol :: Protocol -> Word8
fromProtocol TCP            = 6
fromProtocol UDP            = 17
fromProtocol (Unknown w)    = w
