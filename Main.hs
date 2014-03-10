{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans

import Data.Binary
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Monoid

import Network.Pcap
import Pcap
import Quote

pcapSrc :: PcapHandle -> Link -> Source IO Pcap
pcapSrc h l = do
    mp <- liftIO $ nextPcap h l
    case mp of
        Just p -> do
            yield p
            pcapSrc h l
        Nothing ->
            return ()

pcapDecode :: Monad m => Conduit Pcap m Quote
pcapDecode = CL.concatMap decodePcap
    where
        decodePcap p =
            case decodeOrFail (_payload p) of
                Right (_, _, q@Quote{}) -> [q]
                _ -> []

quotePrinter :: Sink Quote IO ()
quotePrinter = CL.mapM_ $ \q -> do
    let str = BS.intercalate " " $
                [ _acceptTime q, _issueCode q ]
                ++ map pq (reverse . take 5 $ [(bp, bq) | Bid bp bq <- _bids q])
                ++ map pq (take 5 $ [(ap, aq) | Ask ap aq <- _asks q])
    BS.putStrLn str
    where
        pq (p', q') = p' <> "@" <> q'

main :: IO ()
main = do
    h <- openOffline "mdf-kospi200.20110216-0.pcap"
    dl <- Network.Pcap.datalink h
    pcapSrc h dl $$ pcapDecode =$ quotePrinter
