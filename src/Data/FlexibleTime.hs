{-# LANGUAGE OverloadedStrings #-}
module Data.FlexibleTime
       ( (.+)
       , (.-)
       , getYear
       , getMonth
       , getDay
       , getDate
       , getHour
       , getMin
       , getSec
       ) where

import qualified Data.ByteString.Char8 as BS
import           Data.UnixTime         (Format, UnixTime, addUnixDiffTime,
                                        diffUnixTime, formatUnixTime,
                                        secondsToUnixDiffTime, udtSeconds)
import           Foreign.C.Types       (CTime (..))
import           System.IO.Unsafe
----------------------------------------------------------------------

-- Add seconds to UnixTime
(.+) :: (Integral a) => UnixTime -> a -> UnixTime
u .+ d = addUnixDiffTime u (secondsToUnixDiffTime d)

-- Substract seconds from UnixTime
(.-) :: (Integral a) => UnixTime -> UnixTime -> a
l .- r = ctimeToIntegral $ udtSeconds $ diffUnixTime l r

getYear :: (Integral a) => UnixTime -> a
getYear = get "%Y"

getMonth :: (Integral a) => UnixTime -> a
getMonth = get "%m"

getDay :: (Integral a) => UnixTime -> a
getDay = get "%d"

getDate :: (Integral a) => UnixTime -> a
getDate = get "%w"

getHour :: (Integral a) => UnixTime -> a
getHour = get "%H"

getMin :: (Integral a) => UnixTime -> a
getMin = get "%M"

getSec :: (Integral a) => UnixTime -> a
getSec = get "%S"

----------------------------------------------------------------------

ctimeToIntegral :: (Integral a) => CTime -> a
ctimeToIntegral (CTime n) = fromIntegral n

get :: (Integral a) => Format -> UnixTime -> a
get f = readBsNum . unsafePerformIO . formatUnixTime f

readBsNum :: (Integral a) => BS.ByteString -> a
readBsNum bs = go (reverse . map readChar . BS.unpack $ bs) 0 1
  where
    go [] num _ = num
    go (x:xs) num cnt = go xs (num + (x * cnt)) (cnt*10)

readChar :: (Integral a) => Char -> a
readChar '0' = 0
readChar '1' = 1
readChar '2' = 2
readChar '3' = 3
readChar '4' = 4
readChar '5' = 5
readChar '6' = 6
readChar '7' = 7
readChar '8' = 8
readChar '9' = 9
readChar _ = error "parse error"
