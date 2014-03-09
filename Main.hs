module Main where

import Data.Binary
import Network.Pcap
import Pcap
import Quote

pcaploop :: PcapHandle -> Link -> IO ()
pcaploop h l = do
    mp <- nextPcap h l
    case mp of
        Just p -> do
            -- putStrLn $ pretty p
            case decodeOrFail (_payload p) of
                Right (_, _, q@Quote{}) -> print q
                _ -> return ()
            pcaploop h l
        Nothing ->
            return ()

main :: IO ()
main = do
    h <- openOffline "mdf-kospi200.20110216-0.pcap"
    dl <- Network.Pcap.datalink h
    pcaploop h dl
