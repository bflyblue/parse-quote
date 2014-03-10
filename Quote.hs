{-# LANGUAGE OverloadedStrings #-}

module Quote where

import Control.Applicative
import Control.Monad

import Data.Binary
import Data.Binary.Get

import Data.ByteString

data Quote = Quote
    { _acceptTime       :: ByteString
    , _issueCode        :: ByteString
    , _bids             :: [Bid]
    , _asks             :: [Ask]
    }
    | UnsupportedQuote
    deriving (Show)

instance Binary Quote where
    -- Lots of warnings for unused values but here for reference
    get = do
        dataType            <- getByteString 2
        infoType            <- getByteString 2
        marketType          <- getByteString 1
        if  dataType  == "B6" &&
            infoType  == "03" &&
            marketType == "4"
        then do
            issueCode           <- getByteString 12
            issueSeqNo          <- getByteString 3
            marketStatusType    <- getByteString 2
            totalBidQVolume     <- getByteString 7
            bid1                <- get
            bid2                <- get
            bid3                <- get
            bid4                <- get
            bid5                <- get
            totalAskQVolume     <- getByteString 7
            ask1                <- get
            ask2                <- get
            ask3                <- get
            ask4                <- get
            ask5                <- get
            nBestBidValidQTtl   <- getByteString 5
            nBestBidQ1          <- getByteString 4
            nBestBidQ2          <- getByteString 4
            nBestBidQ3          <- getByteString 4
            nBestBidQ4          <- getByteString 4
            nBestBidQ5          <- getByteString 4
            nBestAskValidQTtl   <- getByteString 5
            nBestAskQ1          <- getByteString 4
            nBestAskQ2          <- getByteString 4
            nBestAskQ3          <- getByteString 4
            nBestAskQ4          <- getByteString 4
            nBestAskQ5          <- getByteString 4
            acceptTime          <- getByteString 8
            eom                 <- getWord8
            unless (eom == 0xff) (fail "Invalid Quote")
            return $! Quote acceptTime issueCode [bid1, bid2, bid3, bid4, bid5] [ask1, ask2, ask3, ask4, ask5]
        else do
            return $! UnsupportedQuote

    put = undefined     -- TODO

data Bid = Bid
    { _bidPrice         :: ByteString
    , _bidQuantity      :: ByteString
    } deriving (Show)

instance Binary Bid where
    get = Bid <$> getByteString 5 <*> getByteString 7
    put = undefined     -- TODO

data Ask = Ask
    { _askPrice         :: ByteString
    , _askQuantity      :: ByteString
    } deriving (Show)

instance Binary Ask where
    get = Ask <$> getByteString 5 <*> getByteString 7
    put = undefined     -- TODO
