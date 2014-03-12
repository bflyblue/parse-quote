module Datalink where

import Control.Applicative

import Data.Binary
import Data.Binary.Get
import Data.ByteString.Lazy (ByteString)

import Network.Pcap (Link(..))
import Ethernet

data Datalink = Ethernet !EtherFrame
              | UnsupportedDatalink !Link ByteString
    deriving (Show)

datalink :: Link -> Get Datalink
datalink DLT_EN10MB = ethernet
datalink link       = UnsupportedDatalink <$> pure link <*> getRemainingLazyByteString

ethernet :: Get Datalink
ethernet = Ethernet <$> get
