
-- | Module provides data type for tracked time data
-- and time conversions functions.
module Model.TrackedTime
     ( TrackedTime(..)
     ) where

-- | This type stores data about tracked time.
data TrackedTime = TrackedTime 
  { _hours   :: Int
  , _minutes :: Int
  , _seconds :: Int
  }

instance Show TrackedTime where
  show (TrackedTime hr mi sc) = show hr ++ ":" ++ showHelper mi ++ ":" ++ showHelper sc
    where
      showHelper timeUnit =
        case timeUnit `div` 10 of
        0   -> "0" ++ show timeUnit
        _   -> show timeUnit
