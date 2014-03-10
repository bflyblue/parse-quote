module Time where

import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Monoid

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
mkTime (Time h m s hs) = mkDigits 2 h <> mkDigits 2 m <> mkDigits 2 s <> mkDigits 2 hs
    where
        mkDigits 0 _ = BS.empty
        mkDigits i n = let (d, r) = divMod n 10 in mkDigits (i - 1 :: Int) d <> mkDigit r

        mkDigit = BS.singleton . intToDigit

diffTime :: Time -> Time -> Int
diffTime (Time ah am as ahs) (Time bh bm bs bhs) = (ah - bh) * 360000 + (am - bm) * 6000 + (as - bs) * 100 + (ahs - bhs)
