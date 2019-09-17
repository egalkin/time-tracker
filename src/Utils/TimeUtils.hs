module Utils.TimeUtils where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.System

getCurrentDate :: IO Day
getCurrentDate = utctDay <$> getCurrentTime

getSystemSeconds :: IO Int 
getSystemSeconds = fromIntegral.systemSeconds <$> getSystemTime

secondsInHour :: Int
secondsInHour = 3600 