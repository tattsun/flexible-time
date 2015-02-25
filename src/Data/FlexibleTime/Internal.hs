{-# LANGUAGE OverloadedStrings #-}
module Data.FlexibleTime.Internal where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UnixTime         as UTime
import           Data.Word             ()
import           Foreign.C.Types
----------------------------------------------------------------------

data FlexibleTime = FlexibleTime { ftYear         :: !Integer
                                 , ftMonth        :: !Integer
                                 , ftDay          :: !Integer
                                 , ftHour         :: !Integer
                                 , ftMin          :: !Integer
                                 , ftSec          :: !Integer
                                 , ftTimeSec      :: !Integer
                                 , ftTimeMicroSec :: !Integer
                                 } deriving (Show, Eq)

instance Ord FlexibleTime where
  l `compare` r = let scmp = (ftTimeSec l) `compare` (ftTimeSec r)
                  in if scmp == EQ
                     then (ftTimeMicroSec l) `compare` (ftTimeMicroSec r)
                     else scmp

----------------------------------------------------------------------

utimeToFlexTime :: UTime.UnixTime -> IO FlexibleTime
utimeToFlexTime utime = do
  fmt <- UTime.formatUnixTime "%Y%m%d%H%M%S" utime
  return $ FlexibleTime { ftYear = getnum 0 3 fmt
                        , ftMonth = getnum 4 5 fmt
                        , ftDay = getnum 6 7 fmt
                        , ftHour = getnum 8 9 fmt
                        , ftMin = getnum 10 11 fmt
                        , ftSec = getnum 12 13 fmt
                        , ftTimeSec = utsectoint (UTime.utSeconds utime)
                        , ftTimeMicroSec = fromIntegral
                                           (UTime.utMicroSeconds utime)
                        }
  where
    getnum s e = readBsNum . BS.drop s . BS.take (e+1)
    utsectoint (CTime c) = fromIntegral c

readBsNum :: BS.ByteString -> Integer
readBsNum bs = go (reverse . map readChar . BS.unpack $ bs) 0 1
  where
    go [] num _ = num
    go (x:xs) num cnt = go xs (num + (x * cnt)) (cnt*10)

readChar :: Char -> Integer
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
