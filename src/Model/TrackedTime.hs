{-# LANGUAGE TemplateHaskell #-}

module Model.TrackedTime where

import Model.Types(ContextIO)
import Model.Issue
import Model.Project
import Model.TypesLenses
import Utils.TimeUtils

import Data.Semigroup
import Data.List (foldl')
import Control.Lens
import Control.Lens.Operators

data TrackedTime = TrackedTime {
    _hours   :: Int
  , _minutes :: Int
  , _seconds :: Int
}

instance Show TrackedTime where
  show (TrackedTime hr mi sc) = show hr <> ":" <> showHelper mi <> ":" <> showHelper sc
    where
      showHelper timeUnit = 
        case timeUnit `div` 10 of
        0   -> "0" ++ show timeUnit
        _   -> show timeUnit
        
trackedTimeToInt :: TrackedTime -> Int
trackedTimeToInt time = _hours time * 3600 + _minutes time * 60

makeLenses ''TrackedTime

convertSecondsToTrackedTime :: Int -> TrackedTime
convertSecondsToTrackedTime seconds = do
  let hours    = seconds `div` secondsInHour
  let restTime = seconds - hours * secondsInHour
  TrackedTime { _hours = hours, _minutes = restTime `div` 60, _seconds = restTime `mod` 60}

countIssueTrackedSeconds :: Issue -> IO Int
countIssueTrackedSeconds issue
  | not (issue^.issueTrackingStatus) = return $ issue^.issueTimeRecorded
  | issue^.issueTrackingStatus       = do
      currentTimestamp <- getSystemSeconds
      return $ issue^.issueTimeRecorded + (currentTimestamp - issue^.issueLastTrackTimestamp)

countIssueTrackedTime :: Issue -> IO TrackedTime
countIssueTrackedTime issue = convertSecondsToTrackedTime <$> countIssueTrackedSeconds issue

countProjectTrackedTime :: Project -> IO TrackedTime
countProjectTrackedTime project =
  convertSecondsToTrackedTime <$> foldl' ioIntPlus (pure 0) (map countIssueTrackedSeconds (project^.projectIssues))
    where
      ioIntPlus :: IO Int -> IO Int -> IO Int
      ioIntPlus accum ioInt = do
        s1 <- accum
        s2 <- ioInt
        return (s1 + s2)
