module Main where

import Control.Monad.Trans

import Data.Binary
import Data.Conduit
import Data.Conduit.List as CL
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

printer :: Show s => Sink s IO ()
printer = CL.mapM_ $ putStrLn . show

main :: IO ()
main = do
    h <- openOffline "mdf-kospi200.20110216-0.pcap"
    dl <- Network.Pcap.datalink h
    pcapSrc h dl $$ pcapDecode =$ printer
