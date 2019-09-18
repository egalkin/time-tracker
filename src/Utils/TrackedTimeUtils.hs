module Utils.TrackedTimeUtils where

import Model.TrackedTime
import Model.Issue
import Model.Project
import Model.TypesLenses

import Utils.TimeUtils

import Control.Lens.Operators
import Data.List(foldl')


-- | Converts 'TrackedTime' to seconds time representation.
trackedTimeToInt :: TrackedTime -> Int
trackedTimeToInt time = time^.hours * 3600 + time^.minutes * 60 + time^.seconds

-- | Converts seconds time representation to 'TrackedTime'.
convertSecondsToTrackedTime :: Int -> TrackedTime
convertSecondsToTrackedTime seconds = do
  let hours    = seconds `div` secondsInHour
  let restTime = seconds - hours * secondsInHour
  TrackedTime { _hours = hours, _minutes = restTime `div` 60, _seconds = restTime `mod` 60}

-- | Counts seconds for given 'Issue' instance.
countIssueTrackedSeconds :: Issue -> IO Int
countIssueTrackedSeconds issue
  | not (issue^.issueTrackingStatus) = return $ issue^.issueTimeRecorded
  | issue^.issueTrackingStatus       = do
      currentTimestamp <- getSystemSeconds
      return $ issue^.issueTimeRecorded + (currentTimestamp - issue^.issueLastTrackTimestamp)

-- | Count 'TrackedTime' for given 'Issue' instance.
countIssueTrackedTime :: Issue -> IO TrackedTime
countIssueTrackedTime issue = convertSecondsToTrackedTime <$> countIssueTrackedSeconds issue


countProjectTrackedTime :: Project -> IO TrackedTime
countProjectTrackedTime project = do
  totalSecondsCount <- mapM countIssueTrackedSeconds (project^.projectIssues)
  return $ convertSecondsToTrackedTime (foldl' (+) 0 totalSecondsCount)
