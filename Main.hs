{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans
import Control.Monad.State as State
import Data.Monoid

import Data.Binary (decodeOrFail)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString.Builder

import qualified Data.PQueue.Prio.Min as PQ

import Data.Conduit
import qualified Data.Conduit.List as CL

import Options.Applicative

import Network.Pcap
import Pcap
import Quote
import Time

-- |WINDOW is hard-coded to 3 seconds for now to match requirements
window :: Int
window = 300

-- |Parse and print quote data in pcap files specified on the command-line.
main :: IO ()
main = do
    execParser opts >>= parse
    where
        opts = info (helper <*> options)
             (fullDesc <> progDesc "Parse and print quote messages from market data in FILES")

data Options = Options
    { _files    :: [String] -- ^ Files to parse (in order)
    , _reorder  :: Bool     -- ^ Reorder quotes (within 'window' second window)
    }
    deriving Show

options :: Parser Options
options = Options
    <$> arguments str   (metavar "FILES...")
    <*> switch          (short 'r' <> long "reorder" <> help "Reorder according to quote accept time")

-- |Parse pcap file and print quotes. Reordering is done across files, so files should
-- be specified in order.
parse :: Options -> IO ()
parse o = do
    let dec = if _reorder o then pcapDecode =$= quoteReorder window else pcapDecode
    evalStateT (CL.sourceList (_files o) $$ pcapOffline =$= dec =$ quotePrinter) PQ.empty

-- |Conduit that takes filepaths of offline pcap data and yields pcap packets.
pcapOffline :: MonadIO m => Conduit FilePath m Pcap
pcapOffline = awaitForever $ \filepath -> do
    ph <- liftIO $ openOffline filepath
    dl <- liftIO $ Network.Pcap.datalink ph
    yieldPcap ph dl
    where
        yieldPcap h l = do
            mp <- liftIO $ nextPcap h l
            case mp of
                Just p -> do
                    yield p
                    yieldPcap h l
                Nothing ->
                    return ()

-- |Conduit that takes pcap packets, decodes the payload and yields quotes with it's
-- associated capture time.
pcapDecode :: Monad m => Conduit Pcap m (Time, Quote)
pcapDecode = CL.concatMap decodePcap
    where
        decodePcap p =
            case decodeOrFail (_payload p) of
                Right (_, _, q@Quote{}) -> [(adjustPcapTime p q, q)]
                _ -> []

-- |Ugly function that tries to guess capture time.
-- pcap time is in Unixtime (UCT) but Accept time is in Localtime.
-- We assume they're less than 30 seconds apart
adjustPcapTime :: Pcap -> Quote -> Time
adjustPcapTime packet quote =
    let accept = parseTime . _acceptTime $ quote
        capture = fromUnixTime . hdrTime . _pcapHdr $ packet
        Time ah am  _   _ = accept
        Time  _  _ cs chs = capture
        adjustLow = Time ah am cs chs
        adjustHi  = addTime adjustLow 6000
        delta t = (abs $ diffTime t accept, t)
    in snd $ min (delta adjustLow) (delta adjustHi)

dumpPretty :: (MonadIO m, Pretty a) => Conduit a m a
dumpPretty = CL.mapM $ \a -> do
    liftIO . putStrLn . pretty $ a
    return a

type PQueue = PQ.MinPQueue Time (Time, Quote)

-- |Conduit that takes quotes and their capture times, and tries to reorder them
-- by the quote Accept time. New quotes are inserted into a priority queues using
-- the Accept time as the key. The minumim value in the priority queue is removed
-- when it is 'w' hundredths of a second older than the current capture time, or
-- when no more quotes are available upstream.
quoteReorder :: Monad m => Int -> Conduit (Time, Quote) (StateT PQueue m) (Time, Quote)
quoteReorder w = do
    ma <- await
    case ma of
        Just (t, q) -> do
            flush $ (>w) . diffTime t
            lift $ modify $ PQ.insert (parseTime $ _acceptTime q) (t, q)
            quoteReorder window
        Nothing -> do
            flush $ const True
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

-- |Print quotes.
quotePrinter :: MonadIO m => Sink (Time, Quote) m ()
quotePrinter = CL.mapM_ $ \(t, q) -> do
    let sp = char8 ' '
        pq (p', q') = sp <> byteString p' <> char8 '@' <> byteString q'
        line =
            mkTime t  <>
            sp <> byteString (_acceptTime q) <>
            sp <> byteString (_issueCode q) <>
            mconcat [pq (p, n) | Bid p n <- reverse . take 5 $ _bids q] <>
            mconcat [pq (p, n) | Ask p n <-           take 5 $ _asks q]
    liftIO . LBS.putStrLn . toLazyByteString $ line
