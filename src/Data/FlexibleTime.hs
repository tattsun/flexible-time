module Data.FlexibleTime
       ( -- Functions
         getFlexTime
         -- Types
       , FlexibleTime (..)
       ) where

import qualified Data.UnixTime              as UTime
--
import           Data.FlexibleTime.Internal
----------------------------------------------------------------------

getFlexTime :: IO FlexibleTime
getFlexTime = UTime.getUnixTime >>= utimeToFlexTime
