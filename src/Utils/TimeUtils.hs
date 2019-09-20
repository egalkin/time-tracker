
-- | Module provides functions for work with
-- system time and date.
module Utils.TimeUtils
     ( getCurrentDate
     , getSystemSeconds
     , secondsInHour) where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.System

-- | Returns current date.
getCurrentDate :: IO Day
getCurrentDate = utctDay <$> getCurrentTime

-- | Returns system seconds.
getSystemSeconds :: IO Int 
getSystemSeconds = fromIntegral.systemSeconds <$> getSystemTime

-- | Number of seconds in hour.
secondsInHour :: Int
secondsInHour = 3600 