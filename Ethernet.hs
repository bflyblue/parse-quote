{-# LANGUAGE DeriveGeneric #-}

module Ethernet where

import GHC.Generics

import Control.Applicative

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

data EtherFrame = EtherFrame
                { _source       :: !MacAddress
                , _destination  :: !MacAddress
                , _ethertype    :: !EtherType
                }
                | EtherQFrame
                { _source       :: !MacAddress
                , _destination  :: !MacAddress
                , _tpid         :: !EtherType
                , _tci          :: !Word16
                , _ethertype    :: !EtherType
                }
    deriving (Show, Generic)

instance Binary EtherFrame where
    get = do
        src     <- get
        dest    <- get
        tpid    <- get
        case tpid of
            IEEE802_1Q -> do
                tci <- getWord16be
                etype <- get
                return $! EtherQFrame src dest tpid tci etype
            _ ->
                return $ EtherFrame src dest tpid

data MacAddress = MacAddress !Word8 !Word8 !Word8 !Word8 !Word8 !Word8
    deriving (Show, Generic)

instance Binary MacAddress

data EtherType = IP4
               | IEEE802_1Q
               | Unknown !Word16
    deriving (Show, Generic)

instance Binary EtherType where
    get = toEtherType <$> getWord16be
    put = putWord16be . fromEtherType

toEtherType :: Word16 -> EtherType
toEtherType 0x0800          = IP4
toEtherType 0x8100          = IEEE802_1Q
toEtherType w               = Unknown w

fromEtherType :: EtherType -> Word16
fromEtherType IP4           = 0x0800
fromEtherType IEEE802_1Q    = 0x8100
fromEtherType (Unknown w)   = w
