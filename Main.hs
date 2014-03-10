{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans
import Control.Monad.State as State
import Data.Monoid

import Data.Binary (decodeOrFail)
import qualified Data.ByteString.Char8 as BS

import qualified Data.PQueue.Prio.Min as PQ

import Data.Conduit
import qualified Data.Conduit.List as CL

import Options.Applicative

import Network.Pcap
import Pcap
import Quote
import Time

pcapOffline :: MonadIO m => Conduit String m Pcap
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

pcapDecode :: Monad m => Conduit Pcap m (Time, Quote)
pcapDecode = CL.concatMap decodePcap
    where
        decodePcap p =
            case decodeOrFail (_payload p) of
                Right (_, _, q@Quote{}) -> [(pcapTime p q, q)]
                _ -> []

        pcapTime p q =
            let t = hdrTime . _pcapHdr $ p
                sec = fromIntegral $ t `div` 1000000 `mod` 60
                hsec = fromIntegral $ t `div` 10000 `mod` 100
                Time h m s _ = parseTime (_acceptTime q)
                (h', m') = (h + (m + 1) `div` 60 , (m + 1) `mod` 60)
            in snd $ min (abs (sec - s), Time h m sec hsec) (abs (60 + sec - s), Time h' m' sec hsec)

type PQueue = PQ.MinPQueue Time (Time, Quote)

quoteReorder :: Monad m => Int -> Conduit (Time, Quote) (StateT PQueue m) (Time, Quote)
quoteReorder window = do
    ma <- await
    case ma of
        Just (t, q) -> do
            flush $ (>window) . diffTime t
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

quotePrinter :: MonadIO m => Sink (Time, Quote) m ()
quotePrinter = CL.mapM_ $ \(t, q) -> do
    let line = BS.intercalate " " $
                [ mkTime t, _acceptTime q, _issueCode q ]
                ++ map pq [(p, n) | Bid p n <- reverse . take 5 $ _bids q]
                ++ map pq [(p, n) | Ask p n <-           take 5 $ _asks q]
    liftIO $ BS.putStrLn line
    where
        pq (p', q') = p' <> "@" <> q'

data Options = Options
    { _files    :: [String]
    , _reorder  :: Bool
    }
    deriving Show

options :: Parser Options
options = Options
    <$> arguments str   (metavar "FILES...")
    <*> switch          (short 'r' <> long "reorder" <> help "Reorder according to quote accept time")

parse :: Options -> IO ()
parse o = do
    let dec = if _reorder o then pcapDecode =$= quoteReorder 300 else pcapDecode
    evalStateT (CL.sourceList (_files o) $$ pcapOffline =$= dec =$ quotePrinter) PQ.empty

main :: IO ()
main = do
    execParser opts >>= parse
    where
        opts = info (helper <*> options)
             (fullDesc <> progDesc "Parse and print quote messages from market data in FILES")
