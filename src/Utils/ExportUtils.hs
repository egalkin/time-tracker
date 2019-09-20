module Utils.ExportUtils where

import Model.Issue
import Model.Project
import Utils.TrackedTimeUtils
import Data.List

instance Show Project where
  show project = _projectName project ++ "|" ++ show (_projectCreationDate project) ++ ":\n" ++ concatMap (\is -> "#" ++ show is ++ "\n") (_projectIssues project)

instance Show Issue where
  show issue = intercalate "|"
    [_issueName issue, show (_issuePriority issue), show (_issueCreationDate issue), show $ convertSecondsToTrackedTime (_issueTimeRecorded issue),show (_issueTrackingStatus issue), "["++ _issueDescription issue ++"]"]
