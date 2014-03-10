{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans
import Control.Monad.State as State
import Data.Monoid

import Data.Char
import Data.Binary (decodeOrFail)
import qualified Data.ByteString.Char8 as BS

import qualified Data.PQueue.Prio.Min as PQ

import Data.Conduit
import qualified Data.Conduit.List as CL

import Network.Pcap
import Pcap
import Quote

pcapSrc :: MonadIO m => PcapHandle -> Link -> Source m Pcap
pcapSrc h l = do
    mp <- liftIO $ nextPcap h l
    case mp of
        Just p -> do
            yield p
            pcapSrc h l
        Nothing ->
            return ()

data Time = Time Int Int Int Int deriving (Show, Eq, Ord)

parseTime :: BS.ByteString -> Time
parseTime bt = Time (fi h) (fi m) (fi s) (fi hs)
    where
        fi = fromIntegral
        t = maybe 0 fst $ BS.readInteger bt
        h  = t `div` 1000000 `mod` 100
        m  = t `div`   10000 `mod` 100
        s  = t `div`     100 `mod` 100
        hs =               t `mod` 100

mkTime :: Time -> BS.ByteString
mkTime (Time h m s hs) = (mkDigits 2 h) <> (mkDigits 2 m) <> (mkDigits 2 s) <> (mkDigits 2 hs)
    where
        mkDigits 0 _ = BS.empty
        mkDigits i n = let (d, r) = divMod n 10 in mkDigits (i - 1 :: Int) d <> mkDigit r

        mkDigit = BS.singleton . intToDigit

diffTime :: Time -> Time -> Int
diffTime (Time ah am as ahs) (Time bh bm bs bhs) = (ah - bh) * 360000 + (am - bm) * 6000 + (as - bs) * 100 + (ahs - bhs)

pcapDecode :: Monad m => Conduit Pcap m (Time, Quote)
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
                Time h m s _ = parseTime (_acceptTime q)
                (h', m') = (h + (m + 1) `div` 60 , (m + 1) `mod` 60)
            in snd $ min (abs (sec - s), Time h m sec hsec) (abs (60 + sec - s), Time h' m' sec hsec)

type PQueue = PQ.MinPQueue Time (Time, Quote)

quoteReorder :: Monad m => Int -> Conduit (Time, Quote) (StateT PQueue m) (Time, Quote)
quoteReorder window = do
    ma <- await
    case ma of
        Just (t, q) -> do
            flush ((>300) . diffTime t)
            lift $ modify $ PQ.insert (parseTime $ _acceptTime q) (t, q)
            quoteReorder window
        Nothing -> do
            flush (const True)
            return ()
    where
        flush p = do
            pq <- lift get
            unless (PQ.null pq) $ do
                let (t, q) = PQ.findMin pq
                when (p t) $ do
                    lift $ modify PQ.deleteMin
                    yield q
                    flush p


quotePrinter :: MonadIO m => Sink (Time, Quote) m ()
quotePrinter = CL.mapM_ $ \(t, q) -> do
    let str = BS.intercalate " " $
                [ mkTime t, _acceptTime q, _issueCode q ]
                ++ map pq (reverse . take 5 $ [(p, n) | Bid p n <- _bids q])
                ++ map pq (          take 5 $ [(p, n) | Ask p n <- _asks q])
    liftIO $ BS.putStrLn str
    where
        pq (p', q') = p' <> "@" <> q'

main :: IO ()
main = do
    h <- openOffline "mdf-kospi200.20110216-0.pcap"
    dl <- Network.Pcap.datalink h
    evalStateT (pcapSrc h dl $$ pcapDecode =$= quoteReorder 3 =$ quotePrinter) PQ.empty
