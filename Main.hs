module Main where

import Network.Pcap

import Pcap

pcaploop :: PcapHandle -> Link -> IO ()
pcaploop h l = do
    mp <- nextPcap h l
    case mp of
        Just p -> do
            putStrLn $ pretty p
            pcaploop h l
        Nothing ->
            return ()

main :: IO ()
main = do
    h <- openOffline "mdf-kospi200.20110216-0.pcap"
    dl <- Network.Pcap.datalink h
    pcaploop h dl
