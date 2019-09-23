module Utils.TrackedTimeUtils
     ( convertSecondsToTrackedTime
     , countIssueTrackedSeconds
     , countProjectTrackedTime
     , trackedTimeToInt) where

import Control.Lens.Operators

import Data.List(foldl')

import Model.TrackedTime
import Model.Issue
import Model.Project
import Model.TypesLenses
import Utils.TimeUtils

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
      return $ issue^.issueTimeTracked + (currentTimestamp - choseIssueLastTrackTimestamp issue currentTimestamp)
  | otherwise                        = return 0

-- | If last tracked timestamp is 0 then issue was just parsed
-- and last tracked timestamp is now.
choseIssueLastTrackTimestamp :: Issue -> Int -> Int
choseIssueLastTrackTimestamp issue currentTimestamp
  | issue^.issueLastTrackTimestamp == 0 = currentTimestamp
  | otherwise                           = issue^.issueLastTrackTimestamp
  

-- | Counts 'TrackedTime' for given 'Project' instance.
countProjectTrackedTime :: Project -> IO TrackedTime
countProjectTrackedTime project = do
  totalSecondsCount <- mapM countIssueTrackedSeconds (project^.projectIssues)
  return $ convertSecondsToTrackedTime (foldl' (+) 0 totalSecondsCount)
