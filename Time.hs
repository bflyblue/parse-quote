module Time where

import Data.ByteString.Char8
import Data.Char
import Data.Monoid

data Time = Time Int Int Int Int deriving (Show, Eq, Ord)

-- | Parse bytestring time in HHMMSShh format.
parseTime :: ByteString -> Time
parseTime bt = Time (fi h) (fi m) (fi s) (fi hs)
    where
        fi = fromIntegral
        t = maybe 0 fst $ readInteger bt
        h  = t `div` 1000000 `mod` 100
        m  = t `div`   10000 `mod` 100
        s  = t `div`     100 `mod` 100
        hs =               t `mod` 100

-- | Convert from Unix time
fromUnixTime :: Integral n => n -> Time
fromUnixTime t = Time (fi h) (fi m) (fi s) (fi hs)
    where
        fi = fromIntegral
        h  = t `div` 3600000000 `mod` 24
        m  = t `div`   60000000 `mod` 60
        s  = t `div`    1000000 `mod` 60
        hs = t `div`      10000 `mod` 100

-- | Format a 'Time' as HHMMSShh.
mkTime :: Time -> ByteString
mkTime (Time h m s hs) = mkDigits h <> mkDigits m <> mkDigits s <> mkDigits hs
    where
        mkDigits n = let (t, u) = divMod n 10 in mkDigit t <> mkDigit u
        mkDigit = singleton . intToDigit

-- | Calculate the different betweeen two 'Time' values in hundredths of a second.
diffTime :: Time -> Time -> Int
diffTime (Time ah am as ahs) (Time bh bm bs bhs) =
    (ah - bh) * 360000 +
    (am - bm) * 6000 +
    (as - bs) * 100 +
    (ahs - bhs)

-- | Add hundredths of a second to a 'Time'
addTime :: Time -> Int -> Time
addTime (Time h m s hs) hundredths =
    let (s', hs') = divMod (hs + hundredths) 100
        (m', s'') = divMod (s + s') 60
        (h', m'') = divMod (m + m') 60
        h''       = mod    (h + h') 60
    in  Time h'' m'' s'' hs'

        -- pcapTime p q =
        --     let t = hdrTime . _pcapHdr $ p
        --         sec = fromIntegral $ t `div` 1000000 `mod` 60
        --         hsec = fromIntegral $ t `div` 10000 `mod` 100
        --         Time h m s _ = parseTime (_acceptTime q)
        --         (h', m') = (h + (m + 1) `div` 60 , (m + 1) `mod` 60)
        --     in snd $ min (abs (sec - s), Time h m sec hsec) (abs (60 + sec - s), Time h' m' sec hsec)
