{-# LANGUAGE DeriveGeneric #-}

module UDP where

import GHC.Generics

import Data.Binary

data Packet = Packet
    { _sourcePort           :: !Word16
    , _destinationPort      :: !Word16
    , _length               :: !Word16
    , _checksum             :: !Word16
    } deriving (Show, Generic)

instance Binary Packet
