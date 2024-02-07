module Network.Units where

import Text.Printf (printf)

data Unit = KiB | MiB | GiB | TiB | PiB | EiB | ZiB | YiB
          | KB | MB | GB | TB | PB | EB | ZB | YB
          deriving (Enum, Show, Bounded)

bytesToHumanReadable :: Integral a => a -> Bool -> String
bytesToHumanReadable bytes binaryPrefix = printf "%.2f %s" size (show unit)
  where
    (factor, units) = if binaryPrefix
                      then (1024.0, enumFrom KiB)  -- Binary prefix, 1024 base, explicitly Double
                      else (1000.0, enumFrom KB)   -- Decimal prefix, 1000 base, explicitly Double
    (size, unit) = foldl (\(accSize, accUnit) u -> 
                            if accSize < factor then (accSize, accUnit)
                            else (accSize / factor, u))
                         (fromIntegral bytes :: Double, if binaryPrefix then KiB else KB)  -- Explicitly Double
                         units
