{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans

import Data.Char
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

parseTime :: BS.ByteString -> (Int, Int, Int, Int)
parseTime bt = (fi h, fi m, fi s, fi hs)
    where
        fi = fromIntegral
        t = maybe 0 fst $ BS.readInteger bt
        h  = t `div` 1000000 `mod` 100
        m  = t `div`   10000 `mod` 100
        s  = t `div`     100 `mod` 100
        hs =               t `mod` 100

mkTime :: (Int, Int, Int, Int) -> BS.ByteString
mkTime (h, m, s, hs) = (mkDigits 2 h) <> (mkDigits 2 m) <> (mkDigits 2 s) <> (mkDigits 2 hs)
    where
        mkDigits 0 _ = BS.empty
        mkDigits i n = let (d, r) = divMod n 10 in mkDigits (i - 1 :: Int) d <> mkDigit r

        mkDigit = BS.singleton . intToDigit

pcapDecode :: Monad m => Conduit Pcap m (BS.ByteString, Quote)
pcapDecode = CL.concatMap decodePcap
    where
        decodePcap p =
            case decodeOrFail (_payload p) of
                Right (_, _, q@Quote{}) -> [(pcapTime p q, q)]
                _ -> []

        pcapTime p q =
            let t = hdrTime . _pcapHdr $ p
                usec = fromIntegral $ mod t 60000000
                sec = usec `div` 1000000
                hsec = usec `div` 10000 `mod` 100
                (h, m, s, _) = parseTime (_acceptTime q)
                (h', m') = (h + (m + 1) `div` 60 , (m + 1) `mod` 60)
            in mkTime . snd $ min (abs (sec - s), (h, m, sec, hsec)) (abs (60 + sec - s), (h', m', sec, hsec))

quotePrinter :: Sink (BS.ByteString, Quote) IO ()
quotePrinter = CL.mapM_ $ \(t, q) -> do
    let str = BS.intercalate " " $
                [ t, _acceptTime q, _issueCode q ]
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
