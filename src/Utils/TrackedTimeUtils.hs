module Utils.TrackedTimeUtils 
     ( convertSecondsToTrackedTime
     , countIssueTrackedSeconds
     , countProjectTrackedTime
     , trackedTimeToInt) where

import Model.TrackedTime
import Model.Issue
import Model.Project
import Model.TypesLenses
import Utils.TimeUtils

import Data.List(foldl')

import Control.Lens.Operators


-- | Converts 'TrackedTime' to seconds time representation.
trackedTimeToInt :: TrackedTime -> Int
trackedTimeToInt time = time^.hours * 3600 + time^.minutes * 60 + time^.seconds

-- | Converts seconds time representation to 'TrackedTime'.
convertSecondsToTrackedTime :: Int -> TrackedTime
convertSecondsToTrackedTime secs = do
  let hrs    = secs `div` secondsInHour
  let restTime = secs - hrs * secondsInHour
  TrackedTime { _hours = hrs, _minutes = restTime `div` 60, _seconds = restTime `mod` 60}

-- | Counts seconds for given 'Issue' instance.
countIssueTrackedSeconds :: Issue -> IO Int
countIssueTrackedSeconds issue
  | not (issue^.issueTrackingStatus) = return $ issue^.issueTimeTracked
  | issue^.issueTrackingStatus       = do
      currentTimestamp <- getSystemSeconds
      return $ issue^.issueTimeTracked + (currentTimestamp - issue^.issueLastTrackTimestamp)
  | otherwise                        = return 0  
     
-- | Counts 'TrackedTime' for given 'Project' instance.
countProjectTrackedTime :: Project -> IO TrackedTime
countProjectTrackedTime project = do
  totalSecondsCount <- mapM countIssueTrackedSeconds (project^.projectIssues)
  return $ convertSecondsToTrackedTime (foldl' (+) 0 totalSecondsCount)
